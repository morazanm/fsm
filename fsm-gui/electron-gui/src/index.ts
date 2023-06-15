import { app, BrowserWindow, ipcMain, Menu } from 'electron';
import { unlinkSync } from 'fs';
import { createMenu, tmp } from './menuBar/menu';
import { channels } from './shared/constants';
import path from 'path';
// This allows TypeScript to pick up the magic constants that's auto-generated by Forge's Webpack
// plugin that tells the Electron app where to look for the Webpack-bundled app code (depending on
// whether you're running in development or production).
declare const MAIN_WINDOW_WEBPACK_ENTRY: string;
declare const MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY: string;

let FILES_TO_REMOVE = new Set<string>();

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (require('electron-squirrel-startup')) {
  app.quit();
}

const createWindow = (): void => {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    minHeight: 720,
    minWidth: 1280,
    show: false,
    webPreferences: {
      preload: MAIN_WINDOW_PRELOAD_WEBPACK_ENTRY,
      nodeIntegration: true,
      contextIsolation: false,
      // NOTE: This is needed to allow us to import local files into the gui. Currently
      // this is ok because our app does not access the web at all. However using a
      // custom protocol would be better. See:
      // https://www.electronjs.org/docs/latest/api/protocol#protocolinterceptfileprotocolscheme-handler
      webSecurity: false,
    },
  });

  // register the custom menu
  const menu = Menu.buildFromTemplate(createMenu(mainWindow) as any);

  Menu.setApplicationMenu(menu);

  mainWindow.webContents.send(channels.SAVE_FILE, '/Hio');

  // create a new `splash`-Window
  const splash = new BrowserWindow({
    minHeight: 720,
    minWidth: 1280,
    frame: false,
  });
  splash.loadURL(`file://${__dirname}/../../src/splash.html`);

  mainWindow.loadURL(`file://${__dirname}/index.html`);

  // and load the index.html of the app.
  mainWindow.loadURL(MAIN_WINDOW_WEBPACK_ENTRY);

  ipcMain.on(channels.TRACK_FILE, (event, files: string[]) => {
    FILES_TO_REMOVE = new Set([...FILES_TO_REMOVE, ...files]);
  });

  // Open the DevTools.
  mainWindow.webContents.openDevTools();

  mainWindow.on('ready-to-show', () => {
    splash.destroy();
    mainWindow.show();
  });
};

// Remove GraphViz files that are no longer needed
function removeGvizFiles() {
  console.log('Files to remove', FILES_TO_REMOVE);
  FILES_TO_REMOVE.forEach((file) => {
    try {
      unlinkSync(file);
    } catch (e) {
      console.log(e);
    }

    // .svg files also have a .dot so we will try to remove those as well
    try {
      const dotFile = path.format({
        ...path.parse(file),
        base: '',
        ext: '.dot',
      });
      unlinkSync(dotFile);
    } catch (e) {
      console.log(e);
    }
  });
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow);

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
  removeGvizFiles();
  ipcMain.removeAllListeners(channels.TRACK_FILE);
});

app.on('activate', () => {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.
