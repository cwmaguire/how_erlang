console.log("hi!");

async function getData() {
  const url = "http://main:8080/video_urls";
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }

    const json = await response.json();
    console.log(json);
  } catch (error) {
    console.error(error.message);
  }
}

console.log("Getting JSON");

getData();
