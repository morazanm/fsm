import { Grid, Tooltip, ButtonGroup, Button, useTheme } from '@mui/material';
import {
  PlayArrow as RunIcon,
  ArrowBackIosNew as ArrowBackIosNewIcon,
  ArrowForwardIos as ArrowForwardIosIcon,
} from '@mui/icons-material';
import { FSMAlpha } from '../../types/machine';
import InputRender from './InputRender';

type InputComponentProps = {
  input: FSMAlpha[];
  inputIndex: number;
  runMachine: () => void;
  goNext: () => void;
  goPrev: () => void;
};

const InputComponent = (props: InputComponentProps) => {
  const theme = useTheme();
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
            <Tooltip title="Run Machine" disableInteractive>
              <Button
                variant="outlined"
                color="success"
                size="small"
                onClick={props.runMachine}
              >
                <RunIcon color="success" />
              </Button>
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
                <Button onClick={props.goPrev}>
                  <ArrowBackIosNewIcon />
                </Button>
              </Tooltip>
              <Tooltip title="Next" disableInteractive>
                <Button onClick={props.goNext}>
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
        justifyContent="center"
        alignItems="center"
        xs={11}
      >
        <InputRender input={props.input} inputIndex={props.inputIndex} />
      </Grid>
    </Grid>
  );
};

export default InputComponent;
