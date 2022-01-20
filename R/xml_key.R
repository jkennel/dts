# names of values that change from file to file
#' xml_key
#'
#' @return
#' @export
#'
#' @examples
#' xml_key()
xml_key <- function(type) {
  key_1_ultima <- c(
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
  key_2_ultima <- c(
    '</minDateTimeIndex>',
    '</maxDateTimeIndex>',
    '</acquisitionTime>',
    '</referenceTemperature',
    '</probe1Temperature',
    '</probe2Temperature',
    '</forwardSignalAverages>',
    '</reverseSignalAverages>',
    '</probe1Voltage>',
    '</probe2Voltage>',
    '</referenceProbeVoltage>')
  key_1_xt <- c(
    '<startDateTimeIndex>',
    '<endDateTimeIndex>',
    '<acquisitionTime>',
    '<referenceTemperature uom="degC">',
    '<probe1Temperature uom="degC">',
    '<probe2Temperature uom="degC">',
    '<forwardSignalAverages>',
    '<reverseSignalAverages>',
    '<probe1Voltage>',
    '<probe2Voltage>',
    '<referenceProbeVoltage>')
  key_2_xt <- 
    c('</startDateTimeIndex>',
      '</endDateTimeIndex>',
      '</acquisitionTime>',
      '</referenceTemperature>',
      '</probe1Temperature>',
      '</probe2Temperature>',
      '</forwardSignalAverages>',
      '</reverseSignalAverages>',
      '</probe1Voltage>',
      '</probe2Voltage>',
      '</referenceProbeVoltage>')
  nms <- c(
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
  
  if(type == 'xt') {
    xml_key <- data.table(
      key_1 = key_1_xt,
      key_2 = key_2_xt,
      pattern = paste0(key_1_xt, "(.*?)", key_2_xt),
      names = nms
    )
  } else {
    xml_key <- data.table(
      key_1 = key_1_ultima,
      key_2 = key_2_ultima,
      pattern = paste0(key_1_ultima, "(.*?)", key_2_ultima),
      names = nms
    )
    
  }
  
  xml_key
}

