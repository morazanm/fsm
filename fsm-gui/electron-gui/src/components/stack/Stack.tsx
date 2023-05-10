import {
  Stack as MUIStack,
  Typography,
  Box,
  Divider,
  useTheme,
  Grid,
} from '@mui/material';
import {
  FSMTransition,
  PdaTransition,
  isPdaTransition,
} from '../../types/machine';

type StackProps = {
  currentTransition: FSMTransition | null;
};
const Stack = (props: StackProps) => {
  const theme = useTheme();
  const sxTheme = {
    color: theme.palette.text.primary,
    bgcolor: theme.palette.background.default,
  };
  console.log(props.currentTransition);
  const stackElements =
    props.currentTransition && isPdaTransition(props.currentTransition)
      ? (props.currentTransition as PdaTransition).stack
      : [];
  return (
    <Grid item xs={12}>
      <MUIStack direction="column" alignItems="center" overflow="auto">
        <Typography sx={{ ...sxTheme }} variant="h5">
          Stack
        </Typography>
        <Divider
          sx={{ ...sxTheme, bgcolor: theme.palette.divider }}
          orientation="horizontal"
          flexItem
        />
        <MUIStack
          direction="column"
          overflow="auto"
          style={{ maxHeight: '68vh' }}
        >
          {stackElements.map((a, i) => (
            <Box
              sx={{ paddingRight: '10px', paddingLeft: '10px', ...sxTheme }}
              key={i}
            >
              <Typography variant="h6" display="flex" alignItems="center">
                {a}
              </Typography>
            </Box>
          ))}
        </MUIStack>
      </MUIStack>
    </Grid>
  );
};

export default Stack;
