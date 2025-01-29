var urls;
const MIN_URL_SIZE = 100;

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

  div.moveHandlers = [move_to, move_resizers, update_to];

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

function resize_move_to(elem, event){
  const {parentDiv} = elem;
  const {h, w} = calc_div_h_w(parentDiv, event);
  const isParentTooSmall = is_too_small(h, w);
  if(!isParentTooSmall){
    move_to(elem, event);
  }
}

function move_to(elem, {pageX: x, pageY: y}) {
  const {xCenter, yCenter} = center(elem, x, y);
  elem.style.left = xCenter + 'px';
  elem.style.top = yCenter + 'px';
}

function move_resizers(elem, event){
  const {top, left, width, height} = elem.style;

  const t = Number.parseInt(top);
  const l = Number.parseInt(left);
  const w = Number.parseInt(width);
  const h = Number.parseInt(height);

  const topLeft = {pageX: l, pageY: t};
  const topRight = {pageX: l + w, pageY: t};
  const bottomLeft = {pageX: l, pageY: t + h};
  const bottomRight = {pageX: l + w, pageY: t + h};

  const topLeftResizer = elem.topLeft;
  const topRightResizer = elem.topRight;
  const bottomLeftResizer = elem.bottomLeft;
  const bottomRightResizer = elem.bottomRight;

  move_to(topLeftResizer, topLeft);
  move_to(topRightResizer, topRight);
  move_to(bottomLeftResizer, bottomLeft);
  move_to(bottomRightResizer, bottomRight);
}

function update_to(elem, {pageX: x, pageY: y}){
  const {xCenter, yCenter} = center(elem, x, y);
  elem.urlData.left = xCenter;
  elem.urlData.top = yCenter;
}

function center(elem, x, y){
  const halfWidth = elem.offsetWidth / 2;
  const halfHeight = elem.offsetHeight / 2;
  const xCenter = (x - halfWidth);
  const yCenter = (y - halfHeight);
  return {xCenter: xCenter, yCenter: yCenter};
}

function create_resize_corner_divs(urlData, parentDiv){
  const {name, desc, url, top, left, w, h} = urlData;

  const topleft = {x: left, y: top, is_top: true, is_left: true};
  const topright = {x: left + w, y: top, is_top: true, is_left: false};
  const bottomleft = {x: left, y: top + h, is_top: false, is_left: true};
  const bottomright = {x: left + w, y: top + h, is_top: false, is_left: false};

  parentDiv.topLeft = create_resize_corner_div(topleft, parentDiv);
  parentDiv.topRight = create_resize_corner_div(topright, parentDiv);
  parentDiv.bottomLeft = create_resize_corner_div(bottomleft, parentDiv);
  parentDiv.bottomRight = create_resize_corner_div(bottomright, parentDiv);
}

function create_resize_corner_div({x, y, is_top, is_left}, parentDiv){

  const halfSize = 25;
  const top = y - halfSize;
  const left = x - halfSize;

  const cursor = get_cursor(is_top, is_left);

  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px dashed red";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = (halfSize * 2) + 'px';
  div.style.height = (halfSize * 2) + 'px';
  div.style.cursor = cursor;
  div.className = 'url-resize-div';

  div.parentDiv = parentDiv;
  div.is_top = is_top;
  div.is_left = is_left;
  //div.moveHandlers = [resize_move_to, resize_parent];
  div.moveHandlers = [resize_parent];
  div.upHandlers = [];

  document.body.appendChild(div);

  div.onmousedown = () => follow_mouse(div);

  return div;
}

function get_cursor(is_top, is_left){
  if((is_top && is_left) || (!is_top && !is_left)){
    return 'nwse-resize';
  } else {
    return 'nesw-resize';
  }
}

function resize_parent(elem, event){
  const {parentDiv, is_top, is_left} = elem;
  const {pageX: x, pageY: y} = event;

  if(is_top && is_left){
    resize_parent_top_left(elem, event);
  }else if(is_top && !is_left){
    resize_parent_top_right(elem, event);
  }else if(!is_top && is_left){
    resize_parent_bottom_left(elem, event);
  }else if(!is_top && !is_left){
    resize_parent_bottom_right(elem, event);
  };

  move_resizers(parentDiv, event);
}

function resize_parent_top_left(elem, event){
  const {parentDiv} = elem;
  const {pageX: x, pageY: y} = event;
  const {h, w} = calc_div_h_w(parentDiv, event);
  const isTooSmall = is_too_small(h, w);

  if(!isTooSmall){
    parentDiv.style.height = h + 'px';
    parentDiv.style.width = w + 'px';
    parentDiv.style.top = y + 'px';
    parentDiv.style.left = x + 'px';
  }
}

function is_too_small(h, w){
  return h < MIN_URL_SIZE || w < MIN_URL_SIZE;
}

function calc_div_h_w(elem, event){
  const {pageX: x, pageY: y} = event;

  const {top: t0,
         left: l0,
         width: w0,
         height: h0} = elem.style;

  const t = Number.parseInt(t0);
  const l = Number.parseInt(l0);
  const w = Number.parseInt(w0);
  const h = Number.parseInt(h0);

  const bottom = t + h;
  const right = l + w;

  const dx = x - l;
  const dy = y - t;

  const newHeight = h - dy;
  const newWidth = w - dx;

  return {h: newHeight, w: newWidth};
}

function resize_parent_top_right(elem, event){ }
function resize_parent_bottom_left(elem, event){ }
function resize_parent_bottom_right(elem, event){ }

function query_url_data(){
  const notArray = document.querySelectorAll(".url-div");
  const nodes = Array.from(notArray);
  return nodes.map(({urlData}) => urlData);
}
