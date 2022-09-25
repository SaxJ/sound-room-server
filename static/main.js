const context = new AudioContext();
const socket = new WebSocket("ws://localhost:9160");
const eventTypes = [
    "cheer",
    "clap",
    "cry",
    "laugh",
    "woof",
    "quack",
    "boo",
    "wolf",
    "drum",
    "lame",
];

eventTypes.forEach((type) => {
    document.getElementById(type).addEventListener("click", () => {
        socket.send(type);
    });
});

socket.addEventListener("open", () => {
    document.getElementById("status").innerText = "connected";
});

socket.addEventListener("close", () => {
    document.getElementById("status").innerText = "disconnected";
});

socket.addEventListener("message", (event) => {
    const msg = event.data;
    const audioSource = new Audio(`${msg}.wav`);
    audioSource.play();
});

socket.addEventListener("error", () => {
    document.getElementById("status").innerText = "error";
});
