import { Stack, Grid, useTheme } from '@mui/material';
import { FSMAlpha, MachineType, isTmType } from '../../types/machine';

type InputRenderProps = {
  inputIndex: number;
  input: FSMAlpha[];
  type: MachineType;
};

const commonStyle = { fontSize: 32, marginBottom: '0px', marginTop: '0px' };

const useRenderers = (type: MachineType) => {
  const theme = useTheme();
  if (isTmType(type)) {
    const TMInput = (color: string) => {
      return ({ input, index }: { input: FSMAlpha; index: number }) => (
        <Grid
          container
          direction="column"
          justifyContent="center"
          alignItems="center"
        >
          <Grid item xs={10}>
            <p style={{ ...commonStyle, color: color }}>{input}</p>
          </Grid>
          <Grid item xs={2} sx={{ paddingTop: '10px' }}>
            <p>{index}</p>
          </Grid>
        </Grid>
      );
    };

    return [
      TMInput(theme.palette.primary.main),
      TMInput(theme.palette.text.disabled),
      TMInput(theme.palette.text.primary),
    ];
  } else {
    const CurrentInput = ({ input }: { input: FSMAlpha; index: number }) => {
      return (
        <p style={{ ...commonStyle, color: theme.palette.primary.main }}>
          {input}
        </p>
      );
    };

    const PreviousInput = ({ input }: { input: FSMAlpha; index: number }) => {
      return (
        <p style={{ ...commonStyle, color: theme.palette.text.disabled }}>
          {input}
        </p>
      );
    };

    const RestInput = ({ input }: { input: FSMAlpha; index: number }) => {
      return (
        <p style={{ ...commonStyle, color: theme.palette.text.primary }}>
          {input}
        </p>
      );
    };

    return [CurrentInput, PreviousInput, RestInput];
  }
};

const InputRender = (props: InputRenderProps) => {
  const [CurrentInput, PreviousInput, RestInput] = useRenderers(props.type);
  return (
    <Stack
      justifyContent="left"
      alignItems="center"
      direction="row"
      spacing={2}
    >
      {props.input.map((a, i) => {
        if (i < props.inputIndex) {
          return <PreviousInput key={i} input={a} index={i} />;
        } else if (i === props.inputIndex) {
          return <CurrentInput key={i} input={a} index={i} />;
        } else {
          return <RestInput key={i} input={a} index={i} />;
        }
      })}
    </Stack>
  );
};

export default InputRender;
