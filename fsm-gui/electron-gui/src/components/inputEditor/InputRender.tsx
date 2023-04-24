import { Stack, useTheme } from '@mui/material';
import { FSMAlpha } from '../../types/machine';

type InputRenderProps = {
  inputIndex: number;
  input: FSMAlpha[];
};

const commonStyle = { fontSize: 32, marginBottom: '0px', marginTop: '0px' };

const InputRender = (props: InputRenderProps) => {
  const theme = useTheme();
  const CurrentInput = ({ input }: { input: FSMAlpha }) => {
    return (
      <p style={{ ...commonStyle, color: theme.palette.primary.main }}>
        {input}
      </p>
    );
  };

  const PreviousInput = ({ input }: { input: FSMAlpha }) => {
    return (
      <p style={{ ...commonStyle, color: theme.palette.text.disabled }}>
        {input}
      </p>
    );
  };

  const RestInput = ({ input }: { input: FSMAlpha }) => {
    return (
      <p style={{ ...commonStyle, color: theme.palette.text.primary }}>
        {input}
      </p>
    );
  };
  return (
    <Stack
      justifyContent="center"
      alignItems="center"
      direction="row"
      spacing={2}
    >
      {props.input.map((a, i) => {
        if (i < props.inputIndex) {
          return <PreviousInput input={a} />;
        } else if (i === props.inputIndex) {
          return <CurrentInput input={a} />;
        } else {
          return <RestInput input={a} />;
        }
      })}
    </Stack>
  );
};

export default InputRender;
