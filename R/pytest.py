import os
import time
import warnings
import dask.array as da
from scipy.optimize import minimize
from dtscalibration import read_silixa_files
from dtscalibration.dts_accessor_utils import (
    suggest_cable_shift_double_ended,
    shift_double_ended,
)

# The following line introduces the .dts accessor for xarray datasets
import dtscalibration  # noqa: E401  # noqa: E401
from dtscalibration.variance_stokes import variance_stokes_constant
import matplotlib.pyplot as plt
import numpy as np

filepath = os.path.join("..", "..", "scratch", "dts_calibration", "tests", "data", "channel 3")
warnings.simplefilter("ignore")  # Hide warnings to avoid clutter in the notebook

start = time.perf_counter()
ds_notaligned = read_silixa_files(
    directory=filepath, timezone_netcdf="UTC", file_ext="*.xml"
)
end = time.perf_counter()
end - start

ds_notaligned = ds_notaligned.sel(x=slice(7, 11))  # only calibrate parts of the fiber
sections = {
    "probe1Temperature": [slice(7, 11.0)],  # cold bath
}

start = time.perf_counter()
st_var, resid = variance_stokes_constant(
    ds_notaligned.dts.st, sections, ds_notaligned.dts.acquisitiontime_fw, reshape_residuals=True
)
end = time.perf_counter()
end - start

def ufunc(
    sections=None,
    func=None,
    x_coords=None,
    reference_dataset=None,
    dataarray=None,
    subtract_from_dataarray=None,
    subtract_reference_from_dataarray=False,
    ref_temp_broadcasted=False,
    calc_per="stretch",
    **func_kwargs,
):
    """User function applied to parts of the cable. Super useful,
    many options and slightly
    complicated.

    The function `func` is taken over all the timesteps and calculated
    per `calc_per`. This
    is returned as a dictionary

    Parameters
    ----------
    sections : Dict[str, List[slice]], optional
        If `None` is supplied, `ds.sections` is used. Define calibration
        sections. Each section requires a reference temperature time series,
        such as the temperature measured by an external temperature sensor.
        They should already be part of the DataStore object. `sections`
        is defined with a dictionary with its keywords of the
        names of the reference temperature time series. Its values are
        lists of slice objects, where each slice object is a fiber stretch
        that has the reference temperature. Afterwards, `sections` is stored
        under `ds.sections`.
    func : callable, str
        A numpy function, or lambda function to apply to each 'calc_per'.
    x_coords : xarray.DataArray, optional
        x-coordinates, stored as ds.x. If supplied, returns the x-indices of
        the reference sections.
    reference_dataset : xarray.Dataset or Dict, optional
        Contains the reference temperature timeseries refered to in `sections`.
        Not required if `x_indices`.
    dataarray : xarray.DataArray, optional
        Pass your DataArray of which you want to compute the statistics. Has an
        (x,) dimension or (x, time) dimensions.
    subtract_from_dataarray : xarray.DataArray, optional
        Pass your DataArray of which you want to subtract from `dataarray` before you
        compute the statistics. Has an (x,) dimension or (x, time) dimensions.
    subtract_reference_from_dataarray : bool
        If True the reference temperature according to sections is subtracted from
        dataarray before computing statistics
    ref_temp_broadcasted : bool
        Use if you want to return the reference temperature of shape of the reference
        sections
    calc_per : {'all', 'section', 'stretch'}
    func_kwargs : dict
        Dictionary with options that are passed to func

    Returns:
    --------

    Examples:
    ---------
    1. Calculate the variance of the residuals in the along ALL the\
    reference sections wrt the temperature of the water baths

    >>> tmpf_var = ufunc_per_section_helper(
    >>>     func='var',
    >>>     calc_per='all',
    >>>     dataarray=d['tmpf'],
    >>>     subtract_reference_from_dataarray=True)

    2. Calculate the variance of the residuals in the along PER\
    reference section wrt the temperature of the water baths

    >>> tmpf_var = ufunc_per_section_helper
    >>>     func='var',
    >>>     calc_per='stretch',
    >>>     dataarray=d['tmpf'],
    >>>     subtract_reference_from_dataarray=True)

    3. Calculate the variance of the residuals in the along PER\
    water bath wrt the temperature of the water baths

    >>> tmpf_var = ufunc_per_section_helper(
    >>>     func='var',
    >>>     calc_per='section',
    >>>     dataarray=d['tmpf'],
    >>>     subtract_reference_from_dataarray=True)

    4. Obtain the coordinates of the measurements per section

    >>> locs = ufunc_per_section_helper(
    >>>     func=None,
    >>>     dataarray=d.x,
    >>>     subtract_reference_from_dataarray=False,
    >>>     ref_temp_broadcasted=False,
    >>>     calc_per='stretch')

    5. Number of observations per stretch

    >>> nlocs = ufunc_per_section_helper(
    >>>     func=len,
    >>>     dataarray=d.x,
    >>>     subtract_reference_from_dataarray=False,
    >>>     ref_temp_broadcasted=False,
    >>>     calc_per='stretch')

    6. broadcast the temperature of the reference sections to\
    stretch/section/all dimensions. The value of the reference\
    temperature (a timeseries) is broadcasted to the shape of self[\
    label]. The dataarray is not used for anything else.

    >>> temp_ref = ufunc_per_section_helper(
    >>>     dataarray=d["st"],
    >>>     ref_temp_broadcasted=True,
    >>>     calc_per='all')

    7. x-coordinate index

    >>> ix_loc = ufunc_per_section_helper(x_coords=d.x)

    Note:
    ----
    If `dataarray` or `subtract_from_dataarray` is a Dask array, a Dask
    array is returned else a numpy array is returned
    """
    if not func:

        def func(a):
            """Parameters
            ----------
            a

            Returns:
            -------

            """
            return a

    elif isinstance(func, str) and func == "var":

        def func(a):
            """Parameters
            ----------
            a

            Returns:
            -------

            """
            return np.var(a, ddof=1)

    else:
        assert callable(func)

    assert calc_per in ["all", "section", "stretch"]
    assert "x_indices" not in func_kwargs, "pass x_coords arg instead"

    if x_coords is None and (
        (dataarray is not None and hasattr(dataarray.data, "chunks"))
        or (subtract_from_dataarray and hasattr(subtract_from_dataarray.data, "chunks"))
    ):
        concat = da.concatenate
    else:
        concat = np.concatenate

    out = dict()

    for k, section in sections.items():
        out[k] = []
        for stretch in section:
            if x_coords is not None:
                # get indices from stretches
                assert subtract_from_dataarray is None
                assert not subtract_reference_from_dataarray
                assert not ref_temp_broadcasted
                assert not func_kwargs, "Unsupported kwargs"

                # so it is slicable with x-indices
                _x_indices = x_coords.astype(int) * 0 + np.arange(x_coords.size)
                arg1 = _x_indices.sel(x=stretch).data
                out[k].append(arg1)

            elif (
                subtract_from_dataarray is not None
                and not subtract_reference_from_dataarray
                and not ref_temp_broadcasted
            ):
                # calculate std wrt other series
                arg1 = dataarray.sel(x=stretch).data
                arg2 = subtract_from_dataarray.sel(x=stretch).data
                out[k].append(arg1 - arg2)

            elif (
                subtract_from_dataarray is None
                and subtract_reference_from_dataarray
                and not ref_temp_broadcasted
            ):
                # calculate std wrt reference temperature of the corresponding bath
                arg1 = dataarray.sel(x=stretch).data
                arg2 = reference_dataset[k].data
                out[k].append(arg1 - arg2)

            elif (
                subtract_from_dataarray is None
                and not subtract_reference_from_dataarray
                and ref_temp_broadcasted
            ):
                # Broadcast the reference temperature to the length of the stretch
                arg1 = dataarray.sel(x=stretch).data
                arg2 = da.broadcast_to(reference_dataset[k].data, arg1.shape)
                out[k].append(arg2)

            elif (
                subtract_from_dataarray is None
                and not subtract_reference_from_dataarray
                and not ref_temp_broadcasted
            ):
                # calculate std wrt mean value
                arg1 = dataarray.sel(x=stretch).data
                out[k].append(arg1)

        if calc_per == "stretch":
            out[k] = [func(argi, **func_kwargs) for argi in out[k]]

        elif calc_per == "section":
            # flatten the out_dict to sort them
            start = [i.start for i in section]
            i_sorted = np.argsort(start)
            out_flat_sort = [out[k][i] for i in i_sorted]
            out[k] = func(concat(out_flat_sort), **func_kwargs)

        elif calc_per == "all":
            pass

    if calc_per == "all":
        # flatten the out_dict to sort them
        start = [item.start for sublist in sections.values() for item in sublist]
        i_sorted = np.argsort(start)
        out_flat = [item for sublist in out.values() for item in sublist]
        out_flat_sort = [out_flat[i] for i in i_sorted]
        out = func(concat(out_flat_sort, axis=0), **func_kwargs)

        if hasattr(out, "chunks") and len(out.chunks) > 0 and "x" in dataarray.dims:
            # also sum the chunksize in the x dimension
            # first find out where the x dim is
            ixdim = dataarray.dims.index("x")
            c_old = out.chunks
            c_new = list(c_old)
            c_new[ixdim] = sum(c_old[ixdim])
            out = out.rechunk(c_new)

    return out

def testtest(st, sections, acquisitiontime, reshape_residuals=True):
    """Approximate the variance of the noise in Stokes intensity measurements
    with one value, suitable for small setups.

    * `variance_stokes_constant()` for small setups with small variations in\
    intensity. Variance of the Stokes measurements is assumed to be the same\
    along the entire fiber.

    * `variance_stokes_exponential()` for small setups with very few time\
    steps. Too many degrees of freedom results in an under estimation of the\
    noise variance. Almost never the case, but use when calibrating pre time\
    step.

    * `variance_stokes_linear()` for larger setups with more time steps.\
        Assumes Poisson distributed noise with the following model::

            st_var = a * ds.st + b

        where `a` and `b` are constants. Requires reference sections at
        beginning and end of the fiber, to have residuals at high and low
        intensity measurements.

    The Stokes and anti-Stokes intensities are measured with detectors,
    which inherently introduce noise to the measurements. Knowledge of the
    distribution of the measurement noise is needed for a calibration with
    weighted observations (Sections 5 and 6 of [1]_)
    and to project the associated uncertainty to the temperature confidence
    intervals (Section 7 of [1]_). Two sources dominate the noise
    in the Stokes and anti-Stokes intensity measurements
    (Hartog, 2017, p.125). Close to the laser, noise from the conversion of
    backscatter to electricity dominates the measurement noise. The
    detecting component, an avalanche photodiode, produces Poisson-
    distributed noise with a variance that increases linearly with the
    intensity. The Stokes and anti-Stokes intensities are commonly much
    larger than the standard deviation of the noise, so that the Poisson
    distribution can be approximated with a Normal distribution with a mean
    of zero and a variance that increases linearly with the intensity. At
    the far-end of the fiber, noise from the electrical circuit dominates
    the measurement noise. It produces Normal-distributed noise with a mean
    of zero and a variance that is independent of the intensity.

    Calculates the variance between the measurements and a best fit
    at each reference section. This fits a function to the nt * nx
    measurements with ns * nt + nx parameters, where nx are the total
    number of reference locations along all sections. The temperature is
    constant along the reference sections, so the expression of the
    Stokes power can be split in a time series per reference section and
    a constant per observation location.

    Idea from Discussion at page 127 in Richter, P. H. (1995). Estimating
    errors in least-squares fitting.

    The timeseries and the constant are, of course, highly correlated
    (Equations 20 and 21 in [1]_), but that is not relevant here as only the
    product is of interest. The residuals between the fitted product and the
    Stokes intensity measurements are attributed to the
    noise from the detector. The variance of the residuals is used as a
    proxy for the variance of the noise in the Stokes and anti-Stokes
    intensity measurements. A non-uniform temperature of
    the reference sections results in an over estimation of the noise
    variance estimate because all temperature variation is attributed to
    the noise.

    Parameters
    ----------
    reshape_residuals
    st : DataArray
    sections : Dict[str, List[slice]]

    Returns:
    --------
    I_var : float
        Variance of the residuals between measured and best fit
    resid : array_like
        Residuals between measured and best fit

    Notes:
    ------
    * Because there are a large number of unknowns, spend time on\
    calculating an initial estimate. Can be turned off by setting to False.

    * It is often not needed to use measurements from all time steps. If\
    your variance estimate does not change when including measurements\
    additional time steps, you have included enough measurements.

    References:
    -----------
    .. [1] des Tombe, B., Schilperoort, B., & Bakker, M. (2020). Estimation
        of Temperature and Associated Uncertainty from Fiber-Optic Raman-
        Spectrum Distributed Temperature Sensing. Sensors, 20(8), 2235.
        https://doi.org/10.3390/s20082235

    Examples:
    ---------
    - `Example notebook 4: Calculate variance Stokes intensity measurements\
    <https://github.com/\
    dtscalibration/python-dts-calibration/blob/main/examples/notebooks/\
    04Calculate_variance_Stokes.ipynb>`_

    TODO: Account for varying acquisition times
    """
    assert st.dims[0] == "x", "DataArray is transposed"

    # should maybe be per section. But then residuals
    # seem to be correlated between stretches. I don't know why.. BdT.
    data_dict = da.compute(
        ufunc(sections=sections, dataarray=st, calc_per="stretch")
    )[0]

    var_I, resid = test(data_dict)

    return var_I, resid



def test(data_dict):
    def func_fit(p, xs):
        a = p[:xs, None] * p[None, xs:]
        
        return p[:xs, None] * p[None, xs:]
      
    def func_cost(p, data, xs):
        
        fit = func_fit(p, xs)
        return np.sum((fit - data) ** 2)

    resid_list = []

    for k, v in data_dict.items():
        for vi in v:
            nxs, nt = vi.shape

            npar = nt + nxs

            p1 = np.ones(npar) * vi.mean() ** 0.5
            



            res = minimize(func_cost, p1, args=(vi, nxs), method="Powell")
            assert res.success, "Unable to fit. Try variance_stokes_exponential"
            

            fit = func_fit(res.x, nxs)
            resid_list.append(fit - vi)

    resid = np.concatenate(resid_list)

    # unbiased estimater ddof=1, originally thought it was npar
    var_I = resid.var(ddof=1)

    return var_I, resid


st_var, resid = variance_stokes_constant(
    ds.dts.st, sections, ds.dts.acquisitiontime_fw, reshape_residuals=True
)

start = time.perf_counter()
st_var, resid = testtest(
    ds.dts.st, sections, ds.dts.acquisitiontime_fw, reshape_residuals=False
)
st_var, resid = testtest(
    ds.dts.ast, sections, ds.dts.acquisitiontime_fw, reshape_residuals=False
)
st_var, resid = testtest(
    ds.dts.rst, sections, ds.dts.acquisitiontime_bw, reshape_residuals=False
)
st_var, resid = testtest(
    ds.dts.rast, sections, ds.dts.acquisitiontime_bw, reshape_residuals=False
)
end = time.perf_counter()

