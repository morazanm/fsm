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
  Switch,
} from '@mui/material';
import {
  Edit as EditIcon,
  Abc as AbcIcon,
  TableRows as TableRowIcons,
  SsidChart as SsidChartIcon,
  Check as CheckIcon,
  Palette as PaletteIcon,
  RssFeed as RssFeedIcon,
  RadioButtonUnchecked as RadioButtonUncheckedIcon,
  Loop as LoopIcon,
  Straighten as StraightenIcon,
  LocationOn as EditLocationIcon,
  AccessTime as AccessTimeIcon,
} from '@mui/icons-material';
import { styled } from '@mui/material/styles';
import {
  State,
  MachineType,
  isTmType,
  FSMAlpha,
  FSMRule,
  FSMStackAlpha,
} from '../../types/machine';
import { AlphaModal, GammaModal } from './forms/AlphaModals';
import InputForm from './forms/InputForm';
import StateForm from './forms/StateForm';
import TapeForm from './forms/TapePosition';
import useRuleForm from './forms/RuleForm';
import { Connection } from '../../socket/racketInterface';
import { View } from '../../view/MainView';

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
      toggle: (val: OpenModal): OpenModal => (val === null ? 'input' : null),
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
    {
      icon: <AbcIcon />,
      tooltip: 'Alpha',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'alpha' : null),
    },
  ];

  if (isTmType(type)) {
    actions.push({
      icon: <EditLocationIcon />,
      tooltip: 'Tape Position',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'tapePosn' : null),
    });
  }

  if (type === 'tm-language-recognizer') {
    actions.push({
      icon: <CheckIcon />,
      tooltip: 'Accept State',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'accept' : null),
    });
  }

  if (type === 'pda') {
    actions.push({
      icon: <TableRowIcons />,
      tooltip: 'Gamma',
      toggle: (val: OpenModal): OpenModal => (val === null ? 'gamma' : null),
    });
  }

  return actions;
};

type ViewButtonType = {
  title: string;
  view: View;
  icon: JSX.Element;
};

const useViewButtons = (currentView: View): ViewButtonType[] => {
  const views: ViewButtonType[] = [
    { title: 'Control View', view: 'control', icon: <AccessTimeIcon /> },
    { title: 'Graph View', view: 'graphViz', icon: <SsidChartIcon /> },
  ];
  return views.filter((v) => v.view !== currentView);
};

type MachineEditorProps = {
  toggleTheme: () => void;
  machineType: MachineType;
  alpha: FSMAlpha[];
  setAlpha: (alpha: FSMAlpha[]) => void;
  stackAlpha: FSMStackAlpha[];
  setStackAlpha: (stackAlpha: FSMStackAlpha[]) => void;
  nodead: boolean;
  connection: Connection;
  toggleDead: () => void;
  states: State[];
  setStates: (states: State[]) => void;
  input: FSMAlpha[];
  setInput: (incoming: FSMAlpha[]) => void;
  rules: FSMRule[];
  setRules: (rules: FSMRule[]) => void;
  reconnect: () => void;
  tapePosition: number;
  setTapePosition: (position: number) => void;
  setView: (view: View) => void;
  currentView: View;
};

type OpenModal =
  | 'input'
  | 'state'
  | 'rule'
  | 'tapePosn'
  | 'alpha'
  | 'gamma'
  | 'accept'
  | null;

const MachineEditorComponent = (props: MachineEditorProps) => {
  const [openModal, setOpenModal] = useState<OpenModal>(null);
  const [open, setOpen] = useState(false);
  const [snackMsg, setSnackMsg] = useState('');
  const dialActions = useDialActions(props.machineType);
  const RuleForm = useRuleForm(props.machineType);
  const views = useViewButtons(props.currentView);

  const resetSnack = () => setSnackMsg('');
  const toggleSnack = (msg: string) => setSnackMsg(msg);
  return (
    <Box sx={{ marginRight: 1 }}>
      <ButtonGroup size="small" orientation="vertical">
        {!props.connection.connected && (
          <Tooltip
            title={
              props.connection.status === 'attempting'
                ? 'Trying to Connect'
                : 'Disconnected from FSM'
            }
            placement="left-start"
            disableInteractive
          >
            <IconButton
              color={
                props.connection.status === 'attempting' ? 'primary' : 'error'
              }
              onClick={props.reconnect}
            >
              {props.connection.status === 'done' ? (
                <RssFeedIcon />
              ) : (
                <LoopIcon />
              )}
            </IconButton>
          </Tooltip>
        )}
        <Tooltip title="Toggle Theme" placement="left-start" disableInteractive>
          <IconButton color="primary" onClick={props.toggleTheme}>
            <PaletteIcon />
          </IconButton>
        </Tooltip>
        {views.map((v, i) => {
          return (
            <Tooltip
              key={i}
              title={`Toggle ${v.title}`}
              placement="left-start"
              disableInteractive
            >
              <IconButton color="primary" onClick={() => props.setView(v.view)}>
                {v.icon}
              </IconButton>
            </Tooltip>
          );
        })}
        {(props.machineType === 'dfa' || props.machineType === 'ndfa') && (
          <Tooltip
            title={`Use Dead State: ${props.nodead ? 'Off' : 'On'}`}
            placement="left-start"
            disableInteractive
          >
            <Switch checked={!props.nodead} onClick={props.toggleDead} />
          </Tooltip>
        )}
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
      <RuleForm
        isOpen={openModal === 'rule'}
        toggle={() => setOpenModal(null)}
        machineType={props.machineType}
        rules={props.rules}
        setRules={props.setRules}
        states={props.states}
        alpha={props.alpha}
        toggleSnack={toggleSnack}
      />
      <AlphaModal
        isOpen={openModal === 'alpha'}
        toggle={() => setOpenModal(null)}
        alpha={props.alpha}
        setAlpha={props.setAlpha}
        toggleSnack={toggleSnack}
      />

      <GammaModal
        isOpen={openModal === 'gamma'}
        toggle={() => setOpenModal(null)}
        stackAlpha={props.stackAlpha}
        setStackAlpha={props.setStackAlpha}
        toggleSnack={toggleSnack}
      />

      <TapeForm
        isOpen={openModal === 'tapePosn'}
        toggle={() => setOpenModal(null)}
        tapePosition={props.tapePosition}
        setTapePosition={props.setTapePosition}
        toggleSnack={toggleSnack}
        input={props.input}
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
