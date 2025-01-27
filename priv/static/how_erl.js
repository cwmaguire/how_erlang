var urls;

async function getData() {
  const url = "http://main:8080/video_urls";
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }

    const json = await response.json();
    create_urls(json);
  } catch (error) {
    console.error(error.message);
  }
}

function postData(urlData) {
  console.log(urlData);
  const url = "http://main:8080/video_urls";
  fetch(url, {
    method: "POST",
    body: JSON.stringify(urlData),
    headers: { "Content-Type": "application/json" }
  });
}

console.log("Getting JSON");

getData();

function create_urls(json){
  console.log(json);

  json.forEach(create_url);
}

function create_url(url){
  console.log("creating url");
  console.log(url);
  urlDiv = render_url(url);
  add_drag_and_drop(urlDiv);
}

function render_url(urlData){
  const {name, desc, url, top, left, w, h} = urlData;
  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px solid black";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = w + 'px';
  div.style.height = h + 'px';
  div.innerHTML = `<a href='${url}'>${name}</a><br>${desc}`;
  div.className = 'url-div';
  div.urlData = urlData;
  document.body.appendChild(div);
  return div;
}

function add_drag_and_drop(url){
  console.log("Adding drag and drop");
  url.onmousedown = drag_and_drop;
}

function drag_and_drop({target: elem, pageX: x, pageY: y}) {
  float_above_everything(elem);
  snap_to_mouse(elem, x, y);
  follow_mouse(elem);
}

function float_above_everything(elem){
  elem.style.zIndex = 1000;
}

function snap_to_mouse(elem, x, y){
  move_to(elem, x, y);
}

function follow_mouse(elem){
  document.onmousemove = ({pageX: x, pageY: y}) => move_to(elem, x, y);
  document.onmouseup =
    function() {
      elem.style.zIndex = 0;
      stop_following_mouse();
      postData(query_url_data());
    };
}

function stop_following_mouse(){
    document.onmousemove = null;
    document.onmouseup = null;
}

function move_to(elem, x, y) {
  const halfWidth = elem.offsetWidth / 2;
  const halfHeight = elem.offsetHeight / 2;
  const xCenter = (x - halfWidth);
  const yCenter = (y - halfHeight);
  elem.style.left = xCenter + 'px';
  elem.style.top = yCenter + 'px';
  elem.urlData.left = xCenter;
  elem.urlData.top = yCenter;
}

function query_url_data(){
  const notArray = document.querySelectorAll(".url-div");
  const nodes = Array.from(notArray);
  return nodes.map(({urlData}) => urlData);
}
