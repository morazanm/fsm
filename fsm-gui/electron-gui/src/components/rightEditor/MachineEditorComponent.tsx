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
import MachineEditorModal from './MachineEditorModal';
import InputForm from './Forms/InputForm';

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
  states: State[];
  toggleTheme: () => void;
  addState: (state: State) => void;
  removeState: (state: State) => void;
  machineType: MachineType;
  input: FSMAlpha[];
  setInput: (incomming: FSMAlpha[]) => void;
  alpha: FSMAlpha[];
};

type OpenModal = 'input' | 'state' | 'rule' | 'tapePosn' | null;

const MachineEditorComponent = (props: MachineEditorProps) => {
  const [openModal, setOpenModal] = useState<OpenModal>(null);
  const [open, setOpen] = useState(false);
  const toggleModal = () => setOpen(!open);
  const dialActions = useDialActions(props.machineType);
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
          onOpen={() => setOpen(true)}
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
                setOpenModal(i.toggle(openModal));
                setOpen(false);
              }}
              sx={{ width: 30, height: 31, minHeight: 31 }}
            />
          ))}
        </StyledSpeedDial>
      </ButtonGroup>
      <MachineEditorModal
        isOpen={false}
        onClose={toggleModal}
        addState={props.addState}
        removeState={props.removeState}
        states={props.states}
      />
      <InputForm
        isOpen={openModal === 'input'}
        toggle={() => setOpenModal(null)}
        machineType={props.machineType}
        input={props.input}
        setInput={props.setInput}
        alpha={props.alpha}
      />
    </Box>
  );
};

export default MachineEditorComponent;
