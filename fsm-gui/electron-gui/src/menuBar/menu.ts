import { SaveDialogOptions, dialog, shell } from 'electron';
import { homedir } from 'os';

const FSM_DOC_HOMEPAGE = 'https://morazanm.github.io/fsm/fsm/index.html';
const FSM_ISSUES_PAGE = 'https://github.com/morazanm/fsm/issues';
const RACKET_DOC_HOMEPAGE = 'https://docs.racket-lang.org/';

const isMac = process.platform === 'darwin';

export const template = [
  {
    label: 'File',
    submenu: [
      {
        label: 'Save',
        click: () => {
          const options: SaveDialogOptions = {
            title: 'Save Machine',
            defaultPath: homedir(),
            buttonLabel: 'Save',
            filters: [
              { name: 'rkt', extensions: ['rkt'] },
              { name: 'All Files', extensions: ['*'] },
            ],
          };
          dialog.showSaveDialog(null, options).then(({ filePath }) => {
            //TODO: call Racket backend to save the machine
            console.log(filePath);
          });
        },
      },
    ],
  },
  {
    label: 'Edit',
    submenu: [
      {
        role: 'undo',
      },
      {
        role: 'redo',
      },
      {
        type: 'separator',
      },
      {
        role: 'cut',
      },
      {
        role: 'copy',
      },
      {
        role: 'paste',
      },
    ],
  },

  {
    label: 'View',
    submenu: [
      {
        role: 'reload',
      },
      {
        role: 'toggledevtools',
      },
      {
        type: 'separator',
      },
      {
        role: 'resetzoom',
      },
      {
        role: 'zoomin',
      },
      {
        role: 'zoomout',
      },
      {
        type: 'separator',
      },
      {
        role: 'togglefullscreen',
      },
    ],
  },

  {
    role: 'window',
    submenu: [
      {
        role: 'minimize',
      },
      isMac ? { role: 'close' } : { role: 'quit' },
    ],
  },

  {
    role: 'help',
    submenu: [
      {
        label: 'FSM Documentation',
        click: async () => {
          shell.openExternal(FSM_DOC_HOMEPAGE);
        },
      },
      {
        label: 'Racket Documentation',
        click: async () => {
          shell.openExternal(RACKET_DOC_HOMEPAGE);
        },
      },
      {
        label: 'Submit Bug Report',
        click: async () => {
          shell.openExternal(FSM_ISSUES_PAGE);
        },
      },
    ],
  },
];
