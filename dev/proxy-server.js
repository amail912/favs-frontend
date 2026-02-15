const { createProxyMiddleware } = require("http-proxy-middleware");
const express = require('express');
const path = require('path');
const app = express();
const staticDir = path.join(__dirname, '..', 'static');

app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081/',
  changeOrigin: true
}));
app.use('/static', express.static(staticDir));
app.use(express.static(staticDir));
app.get('*', (_req, res) => {
  res.sendFile(path.join(staticDir, 'index.html'));
});

app.listen(1234, () => {
    console.log('server running on port number : 1234');
})
