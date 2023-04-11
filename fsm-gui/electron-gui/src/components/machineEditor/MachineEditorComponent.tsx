import { useState } from 'react';
import { Box, Tooltip, ButtonGroup, IconButton } from '@mui/material';
import {
  Edit as EditIcon,
  SsidChart as SsidChartIcon,
  Palette as PaletteIcon,
} from '@mui/icons-material';
import { State } from '../../types/machine';
import MachineEditorModal from './MachineEditorModal';

type MachineEditorProps = {
  states: State[];
  addState: (state: State) => void;
  removeState: (state: State) => void;
};

const MachineEditorComponent = (props: MachineEditorProps) => {
  const [open, setOpen] = useState(false);
  const toggleModal = () => setOpen(!open);
  return (
    <Box>
      <ButtonGroup size="small" orientation="vertical">
        <Tooltip title="Edit Machine" placement="left-start">
          <IconButton color="primary" onClick={() => toggleModal()}>
            <EditIcon />
          </IconButton>
        </Tooltip>
        <Tooltip title="Toggle Color Bind Mode" placement="left-start">
          <IconButton color="primary">
            <PaletteIcon />
          </IconButton>
        </Tooltip>
        <Tooltip title="Toggle Graph View" placement="left-start">
          <IconButton color="primary">
            <SsidChartIcon />
          </IconButton>
        </Tooltip>
      </ButtonGroup>
      <MachineEditorModal
        isOpen={open}
        onClose={toggleModal}
        addState={props.addState}
        removeState={props.removeState}
        states={props.states}
      />
    </Box>
  );
};

export default MachineEditorComponent;
