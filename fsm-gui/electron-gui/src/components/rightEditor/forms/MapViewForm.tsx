import React from 'react';
import {
  Dialog,
  DialogContent,
  DialogContentText,
  DialogTitle,
  Grid,
  IconButton,
  Slide,
  Step,
  StepButton,
  StepContent,
  Stepper,
  Typography,
  useTheme,
} from '@mui/material';
import { Close as CloseIcon } from '@mui/icons-material';
import { TransitionProps } from '@mui/material/transitions';
import {
  BasicTransition,
  EndTransition,
  FSMTransition,
  extractInputFromRule,
  isEndTransition,
  isNormalTransition,
} from '../../../types/machine';

const Transition = React.forwardRef(function Transition(
  props: TransitionProps & {
    children: React.ReactElement<any, any>;
  },
  ref: React.Ref<unknown>,
) {
  return <Slide direction="up" ref={ref} {...props} />;
});

type CustomStepperProps = {
  transitions: FSMTransition[];
  currentTransition: FSMTransition;
};
const CustomStepper = (props: CustomStepperProps) => {
  const filteredTransitions: (BasicTransition | EndTransition)[] =
    props.transitions.filter(
      (t) => isNormalTransition(t) || isEndTransition(t),
    ) as (BasicTransition | EndTransition)[];

  const getStateNameFromTransition = (
    t: BasicTransition | EndTransition,
  ): string => {
    if (isNormalTransition(t)) {
      return t.rule.start;
    }
    return t.end;
  };

  return (
    <Stepper nonLinear orientation="vertical">
      {filteredTransitions.map((trans, index) => (
        <Step key={index} completed={false} expanded={true}>
          <StepButton color="inherit" onClick={() => console.log('Hi')}>
            {getStateNameFromTransition(trans)}
          </StepButton>
          {isNormalTransition(trans) && (
            <StepContent>
              <Grid container spacing={2}>
                <Grid item xs={6}>
                  <Typography>{extractInputFromRule(trans.rule)}</Typography>
                </Grid>
                <Grid item xs={6}>
                  <DialogContentText hidden={index !== 0}>
                    Consumed Input: a
                  </DialogContentText>
                </Grid>
              </Grid>
              <br />
            </StepContent>
          )}
        </Step>
      ))}
    </Stepper>
  );
};

type MapViewProps = {
  open: boolean;
  onClose: () => void;
  transitions: FSMTransition[];
  currentTransition: FSMTransition;
};

export const MapView = (props: MapViewProps) => {
  const theme = useTheme();
  return (
    <Dialog
      open={props.open}
      TransitionComponent={Transition}
      keepMounted
      maxWidth="sm"
      onClose={props.onClose}
      aria-describedby="alert-dialog-slide-description"
    >
      <DialogTitle>
        {'Map View'}
        <IconButton
          aria-label="close"
          onClick={props.onClose}
          sx={{
            position: 'absolute',
            right: 8,
            top: 8,
            color: theme.palette.grey[500],
          }}
        >
          <CloseIcon />
        </IconButton>
      </DialogTitle>
      <DialogContent>
        {props.transitions.length > 0 ? (
          <>
            <DialogContentText style={{ paddingBottom: '10px' }}>
              Click on a state transition in order to switch the transition
              being visualized by the GUI. highlighting.
            </DialogContentText>
            <CustomStepper
              transitions={props.transitions}
              currentTransition={props.currentTransition}
            />
          </>
        ) : (
          <DialogContentText id="alert-dialog-slide-description">
            Oh No! There does not appear to be any transitions to view! Please
            run the machine and try again.
          </DialogContentText>
        )}
      </DialogContent>
    </Dialog>
  );
};
