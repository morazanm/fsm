import { Stack, Typography, Box, Divider, useTheme, Grid } from '@mui/material';
import { FSMAlpha, FSMStackAlpha, MachineType } from '../../types/machine';

type LeftEditorProps = {
  alpha: FSMAlpha[];
  stackAlpha: FSMStackAlpha[];
  type: MachineType;
};

const LeftEditor = (props: LeftEditorProps) => {
  const theme = useTheme();
  const sxTheme = {
    color: theme.palette.text.primary,
    bgcolor: theme.palette.background.default,
  };
  const width = props.type === 'pda' ? 6 : 12;
  return (
    <Grid container direction="row" rowSpacing={1}>
      <Grid item xs={width}>
        <Stack direction="column" alignItems="center" overflow="auto">
          <Typography sx={{ ...sxTheme }} variant="h5">
            Σ
          </Typography>
          <Divider
            sx={{ ...sxTheme, bgcolor: theme.palette.divider }}
            orientation="horizontal"
            flexItem
          />
          <Stack
            direction="column"
            overflow="auto"
            style={{ maxHeight: '73vh' }}
          >
            {props.alpha.map((a, i) => (
              <Box
                sx={{ paddingRight: '10px', paddingLeft: '10px', ...sxTheme }}
                key={i}
              >
                <Typography variant="h6" display="flex" alignItems="center">
                  {a}
                </Typography>
              </Box>
            ))}
          </Stack>
        </Stack>
      </Grid>
      {props.type === 'pda' && (
        <Grid item xs={width}>
          <Stack direction="column" alignItems="center" overflow="auto">
            <Typography sx={{ ...sxTheme }} variant="h5">
              Γ
            </Typography>
            <Divider
              sx={{ ...sxTheme, bgcolor: theme.palette.divider }}
              orientation="horizontal"
              flexItem
            />
            <Stack
              direction="column"
              overflow="auto"
              style={{ maxHeight: '73vh' }}
            >
              {props.stackAlpha.map((a, i) => (
                <Box
                  sx={{ paddingRight: '10px', paddingLeft: '10px', ...sxTheme }}
                  key={i}
                >
                  <Typography variant="h6" display="flex" alignItems="center">
                    {a}
                  </Typography>
                </Box>
              ))}
            </Stack>
          </Stack>
        </Grid>
      )}
    </Grid>
  );
};

export default LeftEditor;
