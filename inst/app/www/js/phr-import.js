// Opens the dataset file picker of the Data Import module. Used by the
// "Import Dataset(s)" button and the File > Import Data menu items.
window.iphraImportFile = function (accept) {
  var el = document.getElementById('data_import-file');
  if (!el) return;
  el.value = '';
  el.setAttribute('accept', accept || '.xlsx,.xls,.xlsm,.csv,.txt');
  el.click();
};
