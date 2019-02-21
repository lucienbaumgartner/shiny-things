Shiny.addCustomMessageHandler('userdata', function(log) {
  location.href = location.href + "?parameter=" + log;
  localStorage.setItem("data", log);
  parent.window.postMessage("hello", log);
  console.log(log)
});
