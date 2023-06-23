import { Grid, Tooltip, ButtonGroup, Button, useTheme } from '@mui/material';
import {
  PlayArrow as RunIcon,
  ArrowBackIosNew as ArrowBackIosNewIcon,
  ArrowForwardIos as ArrowForwardIosIcon,
} from '@mui/icons-material';
import {
  FSMAlpha,
  FSMTransition,
  MachineType,
  State,
  isTmType,
} from '../../types/machine';
import InputRender from './InputRender';

type InputComponentProps = {
  input: FSMAlpha[];
  states: State[];
  transitions: FSMTransition[];
  inputIndex: number;
  runMachine: () => void;
  goNext: () => void;
  goPrev: () => void;
  type: MachineType;
  isConnected: boolean;
};

const InputComponent = (props: InputComponentProps) => {
  const theme = useTheme();

  const disableButton = (): [boolean, string] => {
    if (!props.isConnected) {
      return [true, "Disconnected from Racket Backend"]
    }
    if (
      props.type === 'tm-language-recognizer' &&
      props.states.find((s: State) => s.type === 'accept') === undefined
    ) {
      return [true, 'Must add accept state'];
    }
    if (props.input.length === 0) {
      return [true, 'Add Input to run'];
    }
    return [false, 'Run Machine'];
  };

  const [disable, msg] = disableButton();

  return (
    <Grid container direction="row" spacing={1} height="100%">
      <Grid
        item
        xs={1}
        justifyContent="center"
        display="flex"
        sx={{
          borderRight: `1px solid ${theme.palette.divider}`,
          height: '108%',
        }}
      >
        <Grid
          container
          display="flex"
          direction="column"
          justifyContent="center"
          alignItems="center"
        >
          <Grid
            item
            xs={6}
            width="inherit"
            display="flex"
            alignItems="center"
            justifyContent="center"
          >
            <Tooltip title={msg} disableInteractive>
              <span>
                <Button
                  variant="outlined"
                  color="success"
                  size="small"
                  onClick={props.runMachine}
                  disabled={disable}
                >
                  <RunIcon color="success" />
                </Button>
              </span>
            </Tooltip>
          </Grid>
          <Grid
            item
            xs={6}
            width="inherit"
            display="flex"
            alignItems="center"
            justifyContent="center"
          >
            <ButtonGroup
              size="small"
              variant="text"
              aria-label="text button group"
            >
              <Tooltip title="Previous" disableInteractive>
                <Button
                  onClick={props.goPrev}
                  disabled={props.transitions.length === 0}
                >
                  <ArrowBackIosNewIcon />
                </Button>
              </Tooltip>
              <Tooltip title="Next" disableInteractive>
                <Button
                  onClick={props.goNext}
                  disabled={props.transitions.length === 0}
                >
                  <ArrowForwardIosIcon />
                </Button>
              </Tooltip>
            </ButtonGroup>
          </Grid>
        </Grid>
      </Grid>
      <Grid
        item
        display="flex"
        justifyContent={isTmType(props.type) ? 'left' : 'center'}
        alignItems="center"
        xs={11}
        style={
          isTmType(props.type) ? { paddingTop: '0px', paddingLeft: '0px' } : {}
        }
      >
        <InputRender
          input={props.input}
          inputIndex={props.inputIndex}
          type={props.type}
        />
      </Grid>
    </Grid>
  );
};

export default InputComponent;
