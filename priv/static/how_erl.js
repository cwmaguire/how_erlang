var urls;

document.ondblclick = create_new_url;

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

function post_url_data() {
  const urlData = query_url_data();
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

function create_new_url({pageX: x, pageY: y}){
  const urlData = {
    left: x,
    top: y,
    h: 200,
    w: 400,
    desc: 'URL description',
    url: 'http://foo.bar/baz',
    name: 'Foobar'
  };
  create_url(urlData);
  post_url_data();
}

function create_url(urlData){
  console.log("creating url");
  console.log(urlData);
  render_url(urlData);
}

function render_url(urlData){
  const {name, desc, url, top, left, w, h} = urlData;

  const mainDiv = create_main_div(urlData);
  create_resize_corner_divs(urlData, mainDiv);

  return mainDiv;
}

function create_main_div(urlData){
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

  div.upHandlers = [function() {
                      div.style.zIndex = 0;
                      post_url_data();
                    }];

  div.moveHandlers = [move_to];

  document.body.appendChild(div);

  div.onmousedown = () => follow_mouse(div);

  return div;
}

function float_above_everything(elem){
  elem.style.zIndex = 1000;
}

function snap_to_mouse(elem, x, y){
  move_to(elem, x, y);
}

function follow_mouse(elem){
  float_above_everything(elem);
  const moveHandlers = elem.moveHandlers;
  const upHandlers = elem.upHandlers;
  upHandlers.unshift(stop_following_mouse);
  document.onmousemove = apply_event_handlers_fun(elem, moveHandlers);
  document.onmouseup = apply_event_handlers_fun(elem, upHandlers);
}

function apply_event_handlers_fun(elem, eventHandlers){
  return function(event){
    eventHandlers.forEach((eventHandler) => eventHandler(elem, event));
  };
}

function stop_following_mouse(){
    document.onmousemove = null;
    document.onmouseup = null;
}

function move_to(elem, {pageX: x, pageY: y}) {
  const halfWidth = elem.offsetWidth / 2;
  const halfHeight = elem.offsetHeight / 2;
  const xCenter = (x - halfWidth);
  const yCenter = (y - halfHeight);
  elem.style.left = xCenter + 'px';
  elem.style.top = yCenter + 'px';
  elem.urlData.left = xCenter;
  elem.urlData.top = yCenter;
}

function create_resize_corner_divs(urlData, parentDiv){
  const {name, desc, url, top, left, w, h} = urlData;

  const topleft = {x: left, y: top, is_top: true, is_left: true};
  const topright = {x: left + w, y: top, is_top: true, is_left: false};
  const bottomleft = {x: left, y: top + h, is_top: false, is_left: true};
  const bottomright = {x: left + w, y: top + h, is_top: false, is_left: false};

  create_resize_corner_div(topleft, parentDiv);
  create_resize_corner_div(topright, parentDiv);
  create_resize_corner_div(bottomleft, parentDiv);
  create_resize_corner_div(bottomright, parentDiv);

}

function create_resize_corner_div({x, y, is_top, is_left}, parentDiv){

  const halfSize = 25;
  const top = y - halfSize;
  const left = x - halfSize;

  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px dashed red";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = (halfSize * 2) + 'px';
  div.style.height = (halfSize * 2) + 'px';
  div.className = 'url-resize-div';

  div.moveHandlers = [({pageX: x}) => console.log(x)];
  div.upHandlers = [];

  document.body.appendChild(div);

  div.onmouse
}

function query_url_data(){
  const notArray = document.querySelectorAll(".url-div");
  const nodes = Array.from(notArray);
  return nodes.map(({urlData}) => urlData);
}
