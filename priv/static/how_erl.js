"use strict";

const RESIZE_CORNER_SIZE = 20;
const MIN_URL_SIZE = 70;

document.ondblclick = create_new_url;

(async function() {
  const url = "http://localhost:8080/video_urls";
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }

    const json = await response.json();
    json.forEach(create_url);
  } catch (error) {
    console.error(error.message);
  }
})();

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
  const mainDiv = create_main_div(urlData);
  render_content(mainDiv);
  create_resize_corner_divs(mainDiv);
  return mainDiv;
}

function create_main_div(urlData){
  const {top, left, w, h} = urlData;
  const div = document.createElement("div");
  div.urlData = urlData;
  div.style.position = "fixed";
  div.style.border = "1px solid black";
  div.style.borderRadius = "15px";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = w + 'px';
  div.style.height = h + 'px';
  div.style.padding = '10px';
  div.className = 'url-div';

  div.onmousedown = ({pageX: x, pageY: y}) => follow_mouse(div, {x, y});
  div.moveHandlers = [move_to, move_resizers, update_to];
  div.upHandlers = [function() {
                      div.style.zIndex = 0;
                    }];
  div.ondblclick = edit_url;
  document.body.appendChild(div);
  return div;
}

function render_content(elem){
  const {urlData} = elem;
  const {name, desc, url} = urlData;
  elem.innerHTML = `<a href='${url}'>${name}</a><br>${desc}<br>`;
  button(elem, 'copy', copy_url);
  button(elem, 'delete', delete_url);
}

function button(elem, text, onClick){
  const b = document.createElement("button");
  b.textContent = text;
  b.style.margin = '5px';
  b.onclick =
    (event) => {
      event.stopPropagation();
      onClick(elem);
    };
  b.onmousedown = stop_propagation;
  b.onmousemove = stop_propagation;
  elem.appendChild(b);
}

function copy_url({urlData}){
  const Y_BUFFER = 20;
  const newUrlData = structuredClone(urlData);
  newUrlData.left = newUrlData.left + newUrlData.w + Y_BUFFER;
  create_url(newUrlData);
  post_url_data();
}

function delete_url(elem){
  elem.topLeft.remove();
  elem.topRight.remove();
  elem.bottomLeft.remove();
  elem.bottomRight.remove();
  elem.remove();
  post_url_data();
}

function float_above_everything(elem){
  elem.style.zIndex = 1000;
}

function follow_mouse(elem, {x, y}){
  const {xd, yd} = mouse_delta(elem, x, y);
  elem.mouseDelta = {xd, yd};
  float_above_everything(elem);
  const moveHandlers = elem.moveHandlers;
  const upHandlers = [stop_following_mouse,
                      post_url_data,
                      ...elem.upHandlers];
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

function move_to(elem, {x: mouseX, y: mouseY}) {
  const {xd, yd} = elem.mouseDelta;
  elem.style.left = mouseX + xd + 'px';
  elem.style.top = mouseY + yd + 'px';
}

function move_resizers(elem){
  const tlResizer = elem.topLeft;
  const trResizer = elem.topRight;
  const blResizer = elem.bottomLeft;
  const brResizer = elem.bottomRight;

  const cps = corner_points(elem);

  move_to(tlResizer, cps.tl);
  move_to(trResizer, cps.tr);
  move_to(blResizer, cps.bl);
  move_to(brResizer, cps.br);
}

function update_to(elem){
  elem.urlData.left = i(elem.style.left);
  elem.urlData.top = i(elem.style.top);
}

function mouse_delta({style: {top, left}}, x, y){
  const xd = i(left) - x;
  const yd = i(top) - y;
  return {xd, yd};
}

function create_resize_corner_divs(elem){
  const cps = corner_points(elem);
  elem.topLeft = create_resize_corner_div(cps.tl, elem);
  elem.topRight = create_resize_corner_div(cps.tr, elem);
  elem.bottomLeft = create_resize_corner_div(cps.bl, elem);
  elem.bottomRight = create_resize_corner_div(cps.br, elem);
}

function create_resize_corner_div({x, y, is_top, is_left}, parentDiv){
  const halfSize = i(RESIZE_CORNER_SIZE / 2);
  const top = y - halfSize;
  const left = x - halfSize;
  const cursor = get_cursor(is_top, is_left);

  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px dotted #C0C0C0";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = RESIZE_CORNER_SIZE + 'px';
  div.style.height = RESIZE_CORNER_SIZE + 'px';
  div.style.cursor = cursor;
  div.className = 'url-resize-div';
  div.parentDiv = parentDiv;
  div.is_top = is_top;
  div.is_left = is_left;
  div.moveHandlers = [resize_parent];
  div.upHandlers = [];
  div.mouseDelta = {xd: -10, yd: -10};
  div.onmousedown = ({pageX: x, pageY: y}) => follow_mouse(div, {x, y});
  document.body.appendChild(div);
  return div;
}

function corner_points({offsetWidth: w,
                        offsetHeight: h,
                        style: s}){
  const top = i(s.top);
  const left = i(s.left);
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

function get_cursor(is_top, is_left){
  if((is_top && is_left) || (!is_top && !is_left)){
    return 'nwse-resize';
  } else {
    return 'nesw-resize';
  }
}

function resize_parent(elem, event){
  const {parentDiv, is_top, is_left} = elem;

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

function query_url_data(){
  const notArray = document.querySelectorAll(".url-div");
  const nodes = Array.from(notArray);
  return nodes.map(({urlData}) => urlData);
}

function edit_url(event){
  event.stopPropagation();
  const elem = event.target;
  const {desc, url, name} = elem.urlData;

  elem.replaceChildren();
  const nameInput = add_textbox(elem, 'nameInput', name, 'Name: ');
  const descInput = add_textbox(elem, 'descInput', desc, 'Desc: ');
  const urlInput = add_textbox(elem, 'urlInput', url, 'URL: ');

  const saveButton = document.createElement('button');
  saveButton.urlInput = urlInput;
  saveButton.nameInput = nameInput;
  saveButton.descInput = descInput;
  saveButton.textContent = 'save';
  saveButton.onclick = (event) => save(event, elem);
  saveButton.onmousedown = stop_propagation;
  saveButton.onmousemove = stop_propagation;
  elem.appendChild(saveButton);

  const cancelButton = document.createElement('button');
  cancelButton.textContent = 'cancel';
  cancelButton.onclick = (event) => cancel(event, elem);
  cancelButton.onmousedown = stop_propagation;
  cancelButton.onmousemove = stop_propagation;
  elem.appendChild(cancelButton);

  window.getSelection().removeAllRanges();
}

function add_textbox(elem, id, value, labelText){
  const input = document.createElement('input');
  input.id = id;
  input.type = 'text';
  input.size = '20';
  input.value = value;
  input.onmousedown = stop_propagation;

  const label = document.createElement('label');
  label.htmlFor = id;
  label.innerText = labelText;

  elem.appendChild(label);
  elem.appendChild(input);
  elem.appendChild(document.createElement('br'));
  return input;
}

function save(event, elem){
  event.stopPropagation();
  const {urlData} = elem;
  const saveButton = event.target;

  urlData.name = saveButton.nameInput.value;
  urlData.desc = saveButton.descInput.value;
  urlData.url = saveButton.urlInput.value;

  render_content(elem);
  post_url_data();
}

function cancel(event, elem){
  event.stopPropagation();
  render_content(elem);
}

function stop_propagation(event){
  event.stopPropagation();
}

function post_url_data() {
  const urlData = query_url_data();
  console.log(`Writing data: ${urlData}`);
  const url = "http://localhost:8080/video_urls";
  fetch(url, {
    method: "POST",
    body: JSON.stringify(urlData),
    headers: { "Content-Type": "application/json" }
  });
}
