import React from 'react';
import ReactDOM from 'react-dom/client';
import {
  Experimental_CssVarsProvider as CssVarsProvider,
  ThemeProvider,
  createTheme,
  useTheme,
} from '@mui/material/styles';
import { StyledEngineProvider } from '@mui/material';
import MainView from './MainView';

const ColorModeContext = React.createContext({ toggleColorMode: () => {} });

const App = () => {
  const theme = useTheme();
  const colorMode = React.useContext(ColorModeContext);

  return (
    <CssVarsProvider>
      <ColorModeContext.Provider value={colorMode}>
        <ThemeProvider theme={theme}>
          <MainView toggleTheme={colorMode.toggleColorMode} />
        </ThemeProvider>
      </ColorModeContext.Provider>
    </CssVarsProvider>
  );
};

function ToggleColorMode() {
  const [mode, setMode] = React.useState<'light' | 'dark'>('light');
  const colorMode = React.useMemo(
    () => ({
      toggleColorMode: () => {
        setMode((prevMode) => (prevMode === 'light' ? 'dark' : 'light'));
      },
    }),
    [],
  );

  const theme = React.useMemo(
    () =>
      createTheme({
        palette: {
          mode,
        },
        components: {
          MuiButton: {
            styleOverrides: {
              outlined: {
                minWidth: 10,
              },
            },
          },
        },
      }),
    [mode],
  );

  return (
    <ColorModeContext.Provider value={colorMode}>
      <ThemeProvider theme={theme}>
        <App />
      </ThemeProvider>
    </ColorModeContext.Provider>
  );
}

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement,
);

root.render(
  <React.StrictMode>
    <StyledEngineProvider injectFirst>
      <ToggleColorMode />
    </StyledEngineProvider>
  </React.StrictMode>,
);
