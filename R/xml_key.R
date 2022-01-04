# names of values that change from file to file
#' xml_key
#'
#' @return
#' @export
#'
#' @examples
#' xml_key()
xml_key <- function() {
  # data.table(
  #   xml_entry = c(
  #     'start',
  #     'end', 
  #     'acquisition_time',
  #     'reference_temperature',
  #     'probe_1',
  #     'probe_2',
  #     'forward_signal_averages',
  #     'reverse_signal_averages',
  #     'probe_1_voltage',
  #     'probe_2_voltage',
  #     'reference_probe_voltage'),
  #   xt = c(
  #     '<minDateTimeIndex>',
  #     '<maxDateTimeIndex>',
  #     '<acquisitionTime>',
  #     '<referenceTemperature>',
  #     '<probe1Temperature>',
  #     '<probe2Temperature>',
  #     '<forwardSignalAverages>',
  #     '<reverseSignalAverages>',
  #     '<probe1Voltage>',
  #     '<probe2Voltage>',
  #     '<referenceProbeVoltage>'
  #   ),
  #   ultima = c(
  #     '<minDateTimeIndex>',
  #     '<maxDateTimeIndex>',
  #     '<acquisitionTime>',
  #     '<referenceTemperature>',
  #     '<probe1Temperature>',
  #     '<probe2Temperature>',
  #     '<forwardSignalAverages>',
  #     '<reverseSignalAverages>',
  #     '<probe1Voltage>',
  #     '<probe2Voltage>',
  #     '<referenceProbeVoltage>')
  # )
  xml_key <- c(
    '<minDateTimeIndex>',
    '<maxDateTimeIndex>',
    '<acquisitionTime>',
    '<referenceTemperature>',
    '<probe1Temperature>',
    '<probe2Temperature>',
    '<forwardSignalAverages>',
    '<reverseSignalAverages>',
    '<probe1Voltage>',
    '<probe2Voltage>',
    '<referenceProbeVoltage>')
  names(xml_key) <- c(
    'start',
    'end', 
    'acquisition_time',
    'reference_temperature',
    'probe_1',
    'probe_2',
    'forward_signal_averages',
    'reverse_signal_averages',
    'probe_1_voltage',
    'probe_2_voltage',
    'reference_probe_voltage')
  
  xml_key
}

