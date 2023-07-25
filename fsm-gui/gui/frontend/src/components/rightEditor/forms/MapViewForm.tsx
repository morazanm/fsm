import React, { useState } from 'react';
import {
  Dialog,
  DialogContent,
  DialogContentText,
  DialogTitle,
  FormControl,
  IconButton,
  InputLabel,
  MenuItem,
  Select,
  Slide,
  Stack,
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
  FSMTransition,
  isEndTransition,
  isNormalTransition,
  isStartTransition,
  ruleToString,
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
  index: number;
  setTransIndex: (isx: number) => void;
};
const CustomStepper = (props: CustomStepperProps) => {
  const theme = useTheme();

  const transToString = (trans: FSMTransition): string => {
    if (isNormalTransition(trans)) {
      return ruleToString(trans.rule);
    } else if (isStartTransition(trans)) {
      return trans.start;
    } else if (isEndTransition(trans)) {
      return trans.end;
    } else {
      //Unreachable
      return '';
    }
  };
  const getInvDetails = (status: string | boolean) => {
    if (typeof status === 'string') {
      return {
        color: theme.palette.error.main,
        status: 'syntax error',
      } as const;
    } else if (status) {
      return { color: theme.palette.success.main, status: 'pass' } as const;
    } else {
      return { color: theme.palette.error.main, status: 'fail' } as const;
    }
  };

  return (
    <Stepper nonLinear orientation="vertical">
      {props.transitions.map((trans, index) => {
        const { color, status } = getInvDetails(trans.invPass);
        return (
          <Step
            key={index}
            completed={false}
            expanded={true}
            active={index === props.index}
          >
            <StepButton
              color="inherit"
              onClick={() => props.setTransIndex(index)}
            >
              {transToString(trans)}
            </StepButton>
            {trans.invPass !== null && (
              <StepContent>
                <Typography component={'span'}>
                  Invariant Status:{' '}
                  <span style={{ color: color }}>{status}</span>
                </Typography>
              </StepContent>
            )}
          </Step>
        );
      })}
    </Stepper>
  );
};

type MapViewProps = {
  open: boolean;
  onClose: () => void;
  transitions: FSMTransition[];
  currentTransIndex: number;
  setTransIndex: (isx: number) => void;
};

type Status = 'pass' | 'fail' | 'all' | 'error';

const hasSameStatus = (trans: FSMTransition, targetStatus: Status): boolean => {
  return (
    (targetStatus === 'pass' && trans.invPass === true) ||
    (targetStatus === 'fail' && trans.invPass === false) ||
    (targetStatus === 'error' && typeof trans.invPass === 'string') ||
    targetStatus === 'all'
  );
};

export const MapView = (props: MapViewProps) => {
  const theme = useTheme();
  const [invStatus, setInvStatus] = useState<Status>('all');

  const filteredTransitions = props.transitions.filter((t) =>
    hasSameStatus(t, invStatus),
  );
  return (
    <Dialog
      open={props.open}
      TransitionComponent={Transition}
      keepMounted
      maxWidth="sm"
      fullScreen
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
        {props.transitions.length >= 0 ? (
          <Stack spacing={2}>
            <DialogContentText style={{ paddingBottom: '10px' }}>
              Click on a state transition in order to switch the transition
              being visualized by the GUI. highlighting.
            </DialogContentText>
            <FormControl>
              <InputLabel id="demo-simple-select-label">Filter</InputLabel>
              <Select
                labelId="demo-simple-select-label"
                id="demo-simple-select"
                value={invStatus}
                label="Age"
                onChange={(e) => setInvStatus(e.target.value as Status)}
              >
                <MenuItem value={'all'}>All</MenuItem>
                <MenuItem value={'pass'}>Pass</MenuItem>
                <MenuItem value={'fail'}>Fail</MenuItem>
                <MenuItem value={'error'}>Error</MenuItem>
              </Select>
            </FormControl>
            <CustomStepper
              transitions={filteredTransitions}
              index={props.currentTransIndex}
              setTransIndex={props.setTransIndex}
            />
          </Stack>
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
