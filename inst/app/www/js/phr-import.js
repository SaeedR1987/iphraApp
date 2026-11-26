// Opens the dataset file picker of the Data Import module. Used by the
// "Import Dataset(s)" button and the File > Import Data menu items.
// Opens the multi-file XLSFORM picker for "Import Data Collection Tool(s)".
window.iphraImportToolFiles = function () {
  var el = document.getElementById('data_import-tool_files');
  if (!el) return;
  el.value = '';
  el.click();
};

window.iphraImportFile = function (accept) {
  var el = document.getElementById('data_import-file');
  if (!el) return;
  el.value = '';
  el.setAttribute('accept', accept || '.xlsx,.xls,.xlsm,.csv,.txt');
  el.click();
};
