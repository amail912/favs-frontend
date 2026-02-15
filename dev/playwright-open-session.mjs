import { chromium } from 'playwright';
import { createRequire } from 'module';

const require = createRequire(import.meta.url);
const { signupAndSignin } = require('../e2e/support/auth-session.js');

const baseApi = process.env.BASE_API || 'http://localhost:1234/api';
const baseUi = process.env.BASE_UI || 'http://localhost:1234/notes';
const keepAliveMs = Number(process.env.KEEP_ALIVE_MS || 30_000);
const headless = process.env.HEADLESS ? process.env.HEADLESS === 'true' : !process.env.DISPLAY;

const session = await signupAndSignin({ apiBase: baseApi, usernamePrefix: 'pw' });

const browser = await chromium.launch({ headless });
const context = await browser.newContext();
await context.addCookies([
  {
    name: session.cookie.name,
    value: session.cookie.value,
    domain: 'localhost',
    path: '/',
    httpOnly: true,
    secure: false,
    sameSite: 'Lax'
  }
]);

const page = await context.newPage();
await page.goto(baseUi, { waitUntil: 'networkidle' });

console.log(JSON.stringify({
  status: 'ready',
  headless,
  username: session.username,
  password: session.password,
  url: baseUi
}));

setInterval(() => {
  // Keep process/session alive until killed.
}, keepAliveMs);
