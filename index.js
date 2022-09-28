import WebSocket, { WebSocketServer } from "ws";
import validator from "validator";

const wss = new WebSocketServer({ port: 9160 });
wss.on("connection", (ws, req) => {
    console.log(`Connection: ${req.url}`);

    const roomName = req.url.replace("/", "");
    if (!validator.isUUID(roomName)) {
        ws.close();
    }

    ws.on(roomName, (data) => {
        ws.send(data);
    });

    ws.on("message", (data, isBinary) => {
        wss.clients.forEach((client) => {
            if (client.readyState === WebSocket.OPEN) {
                client.emit(roomName, data);
            }
        });
    });
});
