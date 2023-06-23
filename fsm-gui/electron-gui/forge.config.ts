import type { ForgeConfig } from '@electron-forge/shared-types';
import { WebpackPlugin } from '@electron-forge/plugin-webpack';

import { mainConfig } from './webpack.main.config';
import { rendererConfig } from './webpack.renderer.config';

const config: ForgeConfig = {
  packagerConfig: {},
  rebuildConfig: {},
  makers: [
    // Debian Based Systems
    {
      name: '@electron-forge/maker-dmg',
      config: {},
    },
    // Windows
    {
      name: '@electron-forge/maker-squirrel',
      config: {},
    },
    // MacOS
    {
      name: '@electron-forge/maker-deb',
      config: {},
    },
  ],
  publishers: [{
    //@ts-ignore
    name: '@electron-forge/publisher-github',
    config: {
      repository: {
        owner: 'morazanm',
        name: 'fsm-gui',
      },
      prerelease: true,
    },
  }],
  plugins: [
    new WebpackPlugin({
      devContentSecurityPolicy: "connect-src 'self' * 'unsafe-eval'",
      mainConfig,
      renderer: {
        config: rendererConfig,
        entryPoints: [
          {
            html: './src/index.html',
            js: './src/renderer.ts',
            name: 'main_window',
            preload: {
              js: './src/preload.ts',
            },
          },
          // Bundle the loading screen
          {
            html: './src/splash.html',
            js: './src/renderer2.ts',
            name: 'splash_window',
          },
        ],
      },
    }),
  ],
};

export default config;
