var urls;
const MIN_URL_SIZE = 70;

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
  console.log(`Writing data: ${urlData}`);
  const url = "http://main:8080/video_urls";
  fetch(url, {
    method: "POST",
    body: JSON.stringify(urlData),
    headers: { "Content-Type": "application/json" }
  });
}

getData();

function create_urls(json){
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

function copy_url(urlData){
  const Y_BUFFER = 20;
  const newUrlData = structuredClone(urlData);
  newUrlData.left = newUrlData.left + newUrlData.w + Y_BUFFER;
  render_url(newUrlData);
  post_url_data();
}

function delete_url(elem){
  elem.remove();
  post_url_data();
}

function create_url(urlData){
  render_url(urlData);
}

function render_url(urlData){
  const {name, desc, url, top, left, w, h} = urlData;

  const mainDiv = create_main_div(urlData);
  create_resize_corner_divs(mainDiv);

  return mainDiv;
}

function create_main_div(urlData){
  const {name, desc, url, top, left, w, h} = urlData;
  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px solid black";
  div.style.borderRadius = "15px";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = w + 'px';
  div.style.height = h + 'px';
  div.style.padding = '10px';
  div.innerHTML = `<a href='${url}'>${name}</a><br>${desc}`;
  div.className = 'url-div';
  div.urlData = urlData;

  div.upHandlers = [function() {
                      div.style.zIndex = 0;
                    }];

  div.moveHandlers = [move_to, move_resizers, update_to];

  document.body.appendChild(div);

  div.onmousedown = () => follow_mouse(div);
  div.ondblclick = edit_url;

  const copyButton = document.createElement("button");
  copyButton.textContent = 'copy';
  copyButton.onclick =
    (event) => {
      event.stopPropagation();
      copy_url(urlData);
    };
  copyButton.onmousedown = stop_propagation;
  copyButton.onmousemove = stop_propagation;
  div.appendChild(copyButton);

  const deleteButton = document.createElement("button");
  deleteButton.textContent = 'delete';
  deleteButton.onclick =
    (event) => {
      event.stopPropagation();
      delete_url(div);
    };
  deleteButton.onmousedown = stop_propagation;
  deleteButton.onmousemove = stop_propagation;
  div.appendChild(deleteButton);

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
  const upHandlers = elem.upHandlers.slice();
  upHandlers.unshift(stop_following_mouse);
  upHandlers.unshift(post_url_data);
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

function move_to(elem, event) {
  const {pageX: x, pageY: y} = event;
  const {xCenter, yCenter} = center(elem, x, y);
  elem.style.left = xCenter + 'px';
  elem.style.top = yCenter + 'px';
}

function move_resizers(elem){
  const tlResizer = elem.topLeft;
  const trResizer = elem.topRight;
  const blResizer = elem.bottomLeft;
  const brResizer = elem.bottomRight;

  const cps = corner_points(elem);

  move_to(tlResizer, add_mouse_event_fields(cps.tl));
  move_to(trResizer, add_mouse_event_fields(cps.tr));
  move_to(blResizer, add_mouse_event_fields(cps.bl));
  move_to(brResizer, add_mouse_event_fields(cps.br));
}

function add_mouse_event_fields(CornerPoint){
  CornerPoint.pageX = CornerPoint.x;
  CornerPoint.pageY = CornerPoint.y;
  return CornerPoint;
}

function update_to(elem, {pageX: x, pageY: y}){
  const {xCenter, yCenter} = center(elem, x, y);
  elem.urlData.left = xCenter;
  elem.urlData.top = yCenter;
}

function center(elem, x, y){
  const halfWidth = Math.round(elem.offsetWidth / 2);
  const halfHeight = Math.round(elem.offsetHeight / 2);
  const xCenter = (x - halfWidth);
  const yCenter = (y - halfHeight);
  return {xCenter: xCenter, yCenter: yCenter};
}

function create_resize_corner_divs(elem){
  const cps = corner_points(elem);
  elem.topLeft = create_resize_corner_div(cps.tl, elem);
  elem.topRight = create_resize_corner_div(cps.tr, elem);
  elem.bottomLeft = create_resize_corner_div(cps.bl, elem);
  elem.bottomRight = create_resize_corner_div(cps.br, elem);
}

function corner_points({offsetWidth: w,
                        offsetHeight: h,
                        style: s}){
  const top = i(s.top);
  const left = i(s.left);
  const p = i(s.padding);
  const rad = i(s.borderRadius);

  const l = left + rad/3;
  const r = left + w - rad/3;
  const t = top + rad/3;
  const b = top + h - rad/3;

  const tl = {x: l, y: t, is_top: true, is_left: true};
  const tr = {x: r, y: t, is_top: true, is_left: false};
  const bl = {x: l, y: b, is_top: false, is_left: true};
  const br = {x: r, y: b, is_top: false, is_left: false};

  return {tl: tl, tr: tr, bl: bl, br: br};
}

function create_resize_corner_div({x, y, is_top, is_left}, parentDiv){

  const halfSize = 10;
  const top = y - halfSize;
  const left = x - halfSize;

  const cursor = get_cursor(is_top, is_left);

  const div = document.createElement("div");
  div.style.position = "fixed";
  //div.style.border = "1px dashed red";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = (halfSize * 2) + 'px';
  div.style.height = (halfSize * 2) + 'px';
  div.style.cursor = cursor;
  div.className = 'url-resize-div';

  div.parentDiv = parentDiv;
  div.is_top = is_top;
  div.is_left = is_left;
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
    resize_parent_(elem, event, calc_div_h_w_top_left);
  }else if(is_top && !is_left){
    resize_parent_(elem, event, calc_div_h_w_top_right);
  }else if(!is_top && is_left){
    resize_parent_(elem, event, calc_div_h_w_bottom_left);
  }else if(!is_top && !is_left){
    resize_parent_(elem, event, calc_div_h_w_bottom_right);
  };

  move_resizers(parentDiv, event);
}

function resize_parent_({parentDiv}, event, dimsFun){
  const {pageX: x, pageY: y} = event;
  const currDims = dimensions(parentDiv);
  const newDims = dimsFun(currDims, {x: x, y: y});
  const clampedDims = clamp_dims(newDims, currDims);
  update(parentDiv, clampedDims);
}

function dimensions({style: {top: t, left: l, width: w, height: h}}){
  return {t: i(t), l: i(l), w: i(w), h: i(h)};
}

function calc_div_h_w_top_left({t, l, w, h}, {x, y}){
  const bottom = t + h;
  const right = l + w;
  return {t: y, l: x, h: bottom - y, w: right - x};
}

function calc_div_h_w_top_right({t, l, w, h}, {x, y}){
  const bottom = t + h;
  return {t: y, l: l, h: bottom - y, w: x - l};
}

function calc_div_h_w_bottom_left({t, l, w, h}, {x, y}){
  const right = l + w;
  return {t: t, l: x, h: y - t, w: right - x};
}

function calc_div_h_w_bottom_right({t, l, w, h}, {x, y}){
  return {t: t, l: l, h: y - t, w: x - l};
}

function is_too_small({h, w}){
  return h < MIN_URL_SIZE || w < MIN_URL_SIZE;
}

function clamp_dims({t: t2, l: l2, h: h2, w: w2},
                    {t: t1, l: l1, h: h1, w: w1}){
  const clamped = {t: t2, l: l2, h: h2, w: w2};
  if(w2 < MIN_URL_SIZE){
    clamped.w = w1;
    clamped.l = l1;
  }
  if(h2 < MIN_URL_SIZE){
    clamped.h = h1;
    clamped.t = t1;
  }
  return clamped;
}

function i(s){
  return Number.parseInt(s);
}

function update(elem, {t, l, h, w}){
    elem.style.height = h + 'px';
    elem.style.width = w + 'px';
    elem.style.top = t + 'px';
    elem.style.left = l + 'px';

    elem.urlData.h = h;
    elem.urlData.w = w;
    elem.urlData.top = t;
    elem.urlData.left = l;
}

function query_url_data(){
  const notArray = document.querySelectorAll(".url-div");
  const nodes = Array.from(notArray);
  return nodes.map(({urlData}) => urlData);
}

function edit_url(event){
  event.stopPropagation();
  console.log('Double Click!');
  const elem = event.target;
  const {desc, url, name} = elem.urlData;

  const nameInput = document.createElement('input');
  nameInput.id = 'nameInput';
  nameInput.type = 'text';
  nameInput.size = '20';
  nameInput.value = name;
  nameInput.onmousedown = (event) => event.stopPropagation();

  const nameLabel = document.createElement('label');
  nameLabel.htmlFor = 'nameInput';
  nameLabel.innerText = 'Name: ';

  const descInput = document.createElement('input');
  descInput.id = 'descInput';
  descInput.type = 'text';
  descInput.size = '20';
  descInput.value = desc;
  descInput.onmousedown = (event) => event.stopPropagation();

  const descLabel = document.createElement('label');
  descLabel.htmlFor = 'descInput';
  descLabel.innerText = 'Description: ';

  const urlInput = document.createElement('input');
  urlInput.id = 'urlInput';
  urlInput.type = 'text';
  urlInput.size = '20';
  urlInput.value = url;
  urlInput.onmousedown = stop_propagation;

  const urlLabel = document.createElement('label');
  urlLabel.htmlFor = 'urlInput';
  urlLabel.innerText = 'URL: ';

  const saveButton = document.createElement('button');
  saveButton.urlInput = urlInput;
  saveButton.nameInput = nameInput;
  saveButton.descInput = descInput;
  saveButton.textContent = 'save';
  saveButton.onclick = (event) => save(event, elem);
  saveButton.onmousedown = stop_propagation;
  saveButton.onmousemove = stop_propagation;

  const cancelButton = document.createElement('button');
  cancelButton.textContent = 'cancel';
  cancelButton.onclick = (event) => cancel(event, elem);
  cancelButton.onmousedown = stop_propagation;
  cancelButton.onmousemove = stop_propagation;

  //elem.innerHTML = '';
  elem.replaceChildren();
  elem.appendChild(nameLabel);
  elem.appendChild(nameInput);
  elem.appendChild(document.createElement('br'));
  elem.appendChild(descLabel);
  elem.appendChild(descInput);
  elem.appendChild(document.createElement('br'));
  elem.appendChild(urlLabel);
  elem.appendChild(urlInput);
  elem.appendChild(document.createElement('br'));
  elem.appendChild(saveButton);
  elem.appendChild(cancelButton);
  window.getSelection().removeAllRanges();
}

function save(event, elem){
  event.stopPropagation();
  const {desc, url, name} = elem.urlData;
  const saveButton = event.target;

  const newName = saveButton.nameInput.value;
  const newDesc = saveButton.descInput.value;
  const newUrl = saveButton.urlInput.value;

  elem.urlData.name = newName;
  elem.urlData.url = newUrl;
  elem.urlData.desc = newDesc;

  elem.innerHTML = `<a href='${newUrl}'>${newName}</a><br>${newDesc}`;
  post_url_data();
}

function cancel(event, elem){
  const {urlData: {name, desc, url}} = elem;
  event.stopPropagation();
  //const {urlData: {name, desc, url}} = event.target;
  elem.innerHTML = `<a href='${url}'>${name}</a><br>${desc}`;
}

function stop_propagation(event){
  event.stopPropagation();
}
