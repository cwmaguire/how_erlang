console.log("hi!");

async function getData() {
  const url = "http://main:8080/video_urls";
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }

    const json = await response.json();
    render(json);
  } catch (error) {
    console.error(error.message);
  }
}

console.log("Getting JSON");

getData();

function render(json){
  console.log(json);

  json.forEach(render_url);
}

function render_url({name, top, left, w, h}){
  const div = document.createElement("div");
  div.style.position = "fixed";
  div.style.border = "1px solid black";
  div.style.top = top + 'px';
  div.style.left = left + 'px';
  div.style.width = w + 'px';
  div.style.height = h + 'px';
  div.innerText = name;
  document.body.appendChild(div);
}

