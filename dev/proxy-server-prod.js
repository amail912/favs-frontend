const { createProxyMiddleware } = require("http-proxy-middleware");
const express = require('express');
const app = express();

app.use('/api', createProxyMiddleware({
  target: 'https://ns328297.ip-37-187-113.eu/',
  changeOrigin: true
}));
app.use(express.static('static'));

app.listen(1234, () => {
    console.log('server running on port number : 1234');
})
