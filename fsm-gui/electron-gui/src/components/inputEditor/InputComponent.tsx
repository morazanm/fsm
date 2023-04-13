import { Grid, ButtonGroup, Button, useTheme, Typography } from '@mui/material';
import {
  ArrowBackIosNew as ArrowBackIosNewIcon,
  ArrowForwardIos as ArrowForwardIosIcon,
} from '@mui/icons-material';
import { FSMAlpha } from '../../types/machine';

type InputComponentProps = {
  input: FSMAlpha[];
  addInput: (input: FSMAlpha[]) => void;
  clearInput: () => void;
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
          direction="column"
          justifyContent="center"
          alignItems="center"
        >
          <Grid item xs={6}>
            <Typography variant="h5">Input:</Typography>
          </Grid>
          <Grid item xs={6}>
            <ButtonGroup
              size="small"
              variant="outlined"
              aria-label="outlined button group"
            >
              <Button>
                <ArrowBackIosNewIcon />
              </Button>
              <Button>
                <ArrowForwardIosIcon />
              </Button>
            </ButtonGroup>
          </Grid>
        </Grid>
      </Grid>
      <Grid item xs={11}></Grid>
    </Grid>
  );
};

export default InputComponent;
