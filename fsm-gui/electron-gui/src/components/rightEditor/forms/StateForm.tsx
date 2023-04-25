import { useState } from 'react';
import TextField from '@mui/material/TextField';
import Radio from '@mui/material/Radio';
import RadioGroup from '@mui/material/RadioGroup';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import FormControl from '@mui/material/FormControl';
import FormControlLabel from '@mui/material/FormControlLabel';
import GenericForm from './GenericForm';
import {
  MachineType,
  isTmType,
  State,
  StateName,
  StateType,
} from '../../../types/machine';

type InputFormProps = {
  isOpen: boolean;
  machineType: MachineType;
  toggle: () => void;
  toggleSnack: (msg: string) => void;
  states: State[];
  setStates: (states: State[]) => void;
};

const validateState = (incomming: StateName, states: State[]): string => {
  if (states.find((s) => s.name === incomming)) {
    return `State "${incomming}" already exists`;
  }
  // State: An uppercase letter (e.g., A) or a symbol comprised of an uppercase letter,
  // dash, and number (e.g., A-72431).
  const regex = new RegExp(/[A-Z](-\d+)*$/g);
  if (regex.test(incomming)) {
    return '';
  } else {
    return `The state: "${incomming}" is not in propper form`;
  }
};

export default function StateForm(props: InputFormProps) {
  const [state, setState] = useState('');
  const [type, setType] = useState<StateType>('normal');
  const [error, setError] = useState('');

  const addState = () => {
    const incomming = state.trim();
    const msg = validateState(incomming, props.states);
    if (msg != '') {
      setError(msg);
    } else {
      props.setStates([...props.states, { name: incomming, type: type }]);
      setError('');
      props.toggle();
      props.toggleSnack(`State: "${incomming}" was added`);
    }
  };

  const deleteState = () => {
    const incomming = state.trim();
    if (props.states.find((s) => s.name === incomming)) {
      props.setStates(props.states.filter((s) => s.name !== incomming));
      setError('');
      props.toggle();
      props.toggleSnack(`State: "${incomming}"" was deleted`);
    } else {
      setError(`State "${incomming}" does not currenly exist`);
    }
  };
  return (
    <GenericForm
      title={'Add/Remove State'}
      isOpen={props.isOpen}
      onClose={() => {
        setError('');
        props.toggle();
      }}
      onSubmit={addState}
      submitText="Add State"
      closeText="Remove State"
      onCancel={deleteState}
    >
      <DialogContent>
        <DialogContentText>
          Please enter the name of the state that you wish to Add/Remove.
        </DialogContentText>
        <TextField
          autoFocus
          error={error !== ''}
          margin="dense"
          id="name"
          label="State Name"
          type="normal"
          fullWidth
          variant="standard"
          helperText={error}
          onChange={(e) => setState(e.target.value)}
        />
        <FormControl>
          <RadioGroup
            row
            aria-labelledby="demo-radio-buttons-group-label"
            defaultValue="none"
            name="radio-buttons-group"
          >
            <FormControlLabel
              value="start"
              onChange={() => setType('start')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Start"
            />
            <FormControlLabel
              value="final"
              onChange={() => setType('final')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Final"
            />
            <FormControlLabel
              value="startFinal"
              onChange={() => setType('startFinal')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Start & Final"
            />
            <FormControlLabel
              value="none"
              onChange={() => setType('normal')}
              control={
                <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
              }
              label="Normal"
            />
            {isTmType(props.machineType) && (
              <FormControlLabel
                value="accept"
                onChange={() => setType('accept')}
                control={
                  <Radio sx={{ '& .MuiSvgIcon-root': { fontSize: 20 } }} />
                }
                label="Accept"
              />
            )}
          </RadioGroup>
        </FormControl>
      </DialogContent>
    </GenericForm>
  );
}
