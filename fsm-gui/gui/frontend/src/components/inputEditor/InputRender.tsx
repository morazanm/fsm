import { Stack, useTheme } from '@mui/material';
import { FSMAlpha, MachineType, isTmType } from '../../types/machine';

type InputRenderProps = {
  inputIndex: number;
  input: FSMAlpha[];
  type: MachineType;
};

const commonStyle = { fontSize: 32, marginBottom: '0px', marginTop: '0px' };
const gridItemCSS = {
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
};

const useRenderers = (type: MachineType) => {
  const theme = useTheme();
  if (isTmType(type)) {
    const TMInput = (color: string) => {
      return ({ input, index }: { input: FSMAlpha; index: number }) => (
        <div
          style={{
            justifyContent: 'center',
            alignItems: 'center',
            textAlign: 'center',
            border: `solid ${theme.palette.divider}`,
            borderWidth: '0px 1px 0px 0px',
            height: 'inherit',
            display: 'grid',
            gridTemplateRows: '1fr 20px',
            paddingLeft: '5px',
            paddingRight: '5px',
          }}
        >
          <div
            style={{
              ...gridItemCSS,
              borderBottom: '1px solid black',
              height: '100%',
            }}
          >
            <p
              style={{
                ...commonStyle,
                color: color,
              }}
            >
              {input}
            </p>
          </div>
          <div style={{ ...gridItemCSS }}>
            <p>{index}</p>
          </div>
        </div>
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
  const customStyle = isTmType(props.type)
    ? { height: '98%', paddingTop: '0px' }
    : {};
  return (
    <Stack
      justifyContent="left"
      alignItems="center"
      direction="row"
      spacing={isTmType(props.type) ? 0 : 2}
      style={{ ...customStyle, overflowY: 'hidden', overflowX: 'auto' }}
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
