import { useState } from 'react';
import {
  Box,
  Tooltip,
  ButtonGroup,
  IconButton,
  SpeedDial,
  SpeedDialAction,
  SpeedDialIcon,
  Backdrop,
  Snackbar,
  Alert,
} from '@mui/material';
import {
  Edit as EditIcon,
  SsidChart as SsidChartIcon,
  Palette as PaletteIcon,
  RadioButtonUnchecked as RadioButtonUncheckedIcon,
  Straighten as StraightenIcon,
  LocationOn as EditLocationIcon,
} from '@mui/icons-material';
import { styled } from '@mui/material/styles';
import { State, MachineType, isTmType, FSMAlpha } from '../../types/machine';
import InputForm from './forms/InputForm';
import StateForm from './forms/StateForm';

const StyledSpeedDial = styled(SpeedDial)(({ theme }) => ({
  '& .MuiFab-primary': {
    '& .MuiSpeedDialIcon-icon': { fontSize: 20 },
    '& .MuiSpeedDialIcon-openIcon': { fontSize: 20 },
    width: 35,
    height: 35,
  },
  '&.MuiSpeedDial-directionUp, &.MuiSpeedDial-directionLeft': {
    bottom: theme.spacing(2),
    right: theme.spacing(2),
  },
  '&.MuiSpeedDial-directionDown, &.MuiSpeedDial-directionRight': {
    top: theme.spacing(2),
    left: theme.spacing(2),
  },
  '& .MuiSpeedDialIcon-root': {
    alignItems: 'center',
    display: 'flex',
  },
}));

const StyledSpeedDialAction = styled(SpeedDialAction)(() => ({
  '& .MuiSvgIcon-root': {
    fontSize: 17,
  },
}));

const useDialActions = (type: MachineType) => {
  const actions = [
    {
      icon: <EditIcon />,
      tooltip: isTmType(type) ? 'Tape Input' : 'Input',
      toggle: (val: OpenModal) => (val === null ? 'input' : null),
    },
    {
      icon: <RadioButtonUncheckedIcon />,
      tooltip: 'State',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'state' : null),
    },
    {
      icon: <StraightenIcon />,
      tooltip: 'Rule',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'rule' : null),
    },
  ];

  if (!isTmType(type)) {
    actions.push({
      icon: <EditLocationIcon />,
      tooltip: 'Tape Position',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'tapePosn' : null),
    });
  }

  return actions;
};

type MachineEditorProps = {
  toggleTheme: () => void;
  machineType: MachineType;
  alpha: FSMAlpha[];
  states: State[];
  setStates: (states: State[]) => void;
  input: FSMAlpha[];
  setInput: (incomming: FSMAlpha[]) => void;
};

type OpenModal = 'input' | 'state' | 'rule' | 'tapePosn' | null;

const MachineEditorComponent = (props: MachineEditorProps) => {
  const [openModal, setOpenModal] = useState<OpenModal>(null);
  const [open, setOpen] = useState(false);
  const [snackMsg, setSnackMsg] = useState('');
  const dialActions = useDialActions(props.machineType);

  const resetSnack = () => setSnackMsg('');
  const toggleSnack = (msg: string) => setSnackMsg(msg);
  return (
    <Box sx={{ marginRight: 1 }}>
      <ButtonGroup size="small" orientation="vertical">
        <Tooltip title="Toggle Theme" placement="left-start" disableInteractive>
          <IconButton color="primary" onClick={props.toggleTheme}>
            <PaletteIcon />
          </IconButton>
        </Tooltip>
        <Tooltip
          title="Toggle Graph View"
          placement="left-start"
          disableInteractive
        >
          <IconButton color="primary">
            <SsidChartIcon />
          </IconButton>
        </Tooltip>
        <Backdrop open={open} />
        <StyledSpeedDial
          ariaLabel="SpeedDial"
          direction="down"
          open={open}
          onMouseEnter={() => setOpen(!open)}
          onClick={() => setOpen(!open)}
          onClose={() => setOpen(false)}
          sx={{ paddingTop: 1 }}
          icon={<SpeedDialIcon openIcon={<EditIcon />} />}
        >
          {dialActions.map((i) => (
            <StyledSpeedDialAction
              key={i.tooltip}
              icon={i.icon}
              tooltipTitle={i.tooltip}
              onClick={() => {
                setOpen(false);
                setOpenModal(i.toggle(openModal));
              }}
              sx={{ width: 30, height: 31, minHeight: 31 }}
            />
          ))}
        </StyledSpeedDial>
      </ButtonGroup>
      <InputForm
        isOpen={openModal === 'input'}
        toggle={() => setOpenModal(null)}
        machineType={props.machineType}
        input={props.input}
        setInput={props.setInput}
        alpha={props.alpha}
      />
      <StateForm
        isOpen={openModal === 'state'}
        toggle={() => setOpenModal(null)}
        machineType={props.machineType}
        setStates={props.setStates}
        states={props.states}
        toggleSnack={toggleSnack}
      />
      {snackMsg && (
        <Snackbar
          open={snackMsg !== ''}
          autoHideDuration={4000}
          onClose={resetSnack}
        >
          <Alert onClose={resetSnack} severity="success" sx={{ width: '100%' }}>
            {snackMsg}
          </Alert>
        </Snackbar>
      )}
    </Box>
  );
};

export default MachineEditorComponent;
