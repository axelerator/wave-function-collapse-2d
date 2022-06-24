
function loadElm() {
  window.Elm = undefined;
  const scriptTag = mkScriptTag('elm-include', 'elm.js');
  scriptTag.addEventListener('load', function() {
    initElm();
  });
}

function initElm() {
  const main = document.querySelector('main')
  while (main.firstChild) {
    main.removeChild(main.lastChild);
  }
  var main_content = document.createElement("div");
  main_content.setAttribute('id', main_content);
  main.appendChild(main_content);

  const app = Elm.Main.init({
    node: main_content
  });
}

const hotReloadInterval = window.setInterval(checkTimestamp, 1000)
window.lastTimestamp = '';
const parsedUrl = new URL(window.location.href);
const pathPrefix = parsedUrl.pathname.replace('/index.html', '');


function buildError(error) {
  const outerDiv = document.createElement("div"); 
  const path = error.path.replace(pathPrefix, '');
  const newContent = document.createTextNode(path);
  outerDiv.appendChild(newContent);

  const problems = document.createElement("ul"); 
  error.problems.forEach(function(p){
    const li = document.createElement('li');
    const pre = document.createElement('pre');
    li.appendChild(pre);
    p.message.forEach(function(message) {
      if ((typeof message) == 'string') {
        pre.appendChild(document.createTextNode(message));
      } else {
        const span = document.createElement('span');
        span.setAttribute("style", "color:" + message.color);
        span.appendChild(document.createTextNode(message.string));
        pre.appendChild(span);
      }
    });
    problems.appendChild(li);
  });

  outerDiv.appendChild(problems);
  return outerDiv;
}

function showErrors(errors) {
  const oldScript = document.getElementById('build-errors');
  if (oldScript) {
    oldScript.remove();
  }
  if (!errors) return;
  const newScript = document.createElement("div"); 
  newScript.setAttribute("id", "build-errors");

  errors.errors.forEach(function(e){
    newScript.appendChild(buildError(e));
  });
  // add the newly created element and its content into the DOM 
  document.getElementById('build-info').appendChild(newScript);
}

function refresh(timestamp, errors) {
  if (window.lastTimestamp == '') {
    if (errors) { 
      showErrors(errors);
    }
  } else if (window.lastTimestamp != timestamp) {
    showErrors(errors);
    if (!errors) {
      loadElm();
    }
  }
  window.lastTimestamp = timestamp;
}

function mkScriptTag(id, filename) {
  const oldScript = document.getElementById(id);
  if (oldScript) {
    oldScript.remove();
  }
  const newScript = document.createElement("script"); 

  newScript.setAttribute("id", id);
  newScript.setAttribute("type", "text/javascript");
  newScript.setAttribute("src", filename );

  // add the newly created element and its content into the DOM 
  document.body.appendChild(newScript);
  return newScript;
}

function checkTimestamp () { 
  mkScriptTag('timestampjs',"tmp/timestamp.js");
}
  
loadElm();

