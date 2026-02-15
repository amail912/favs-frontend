const { createProxyMiddleware } = require("http-proxy-middleware");
const express = require('express');
const app = express();

app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081/',
  changeOrigin: true
}));
app.use(express.static('static'));

app.listen(1234, () => {
    console.log('server running on port number : 1234');
})
