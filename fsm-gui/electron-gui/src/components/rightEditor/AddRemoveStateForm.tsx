import { useState } from 'react';
import { Delete, Add } from '@mui/icons-material';
import {
  Button,
  Input,
  FormControl,
  FormHelperText,
  Stack,
  Box,
  InputLabel,
  RadioGroup,
  FormControlLabel,
  Radio,
} from '@mui/material';
import { State, StateType } from '../../types/machine';

type AddRemoveProps = {
  states: State[];
  onDelete: (state: State) => void;
  onSubmit: (state: State) => void;
  toggleSnack: (text: string) => void;
};

// form for adding and removing adding and removing states from the GUI
const AddRemoveStateForm = (props: AddRemoveProps) => {
  const [val, setValue] = useState('');
  const [errorMsg, setErrorMsg] = useState('');
  const [isInValid, setInValid] = useState(false);
  const [stateType, setType] = useState<StateType>('normal');
  const resetValue = () => setValue('');

  // State: An uppercase letter (e.g., A) or a symbol comprised of an uppercase letter,
  // dash, and number (e.g., A-72431).
  const validateState = (text: string, func: (t: string) => boolean) => {
    const trimmedText = text.trim();
    if (trimmedText && trimmedText.split(' ').length === 1) {
      const hasUpper =
        trimmedText.charAt(0) === trimmedText.charAt(0).toUpperCase();
      if (hasUpper && trimmedText.length === 1 && func(trimmedText)) {
        setInValid(false);
        setErrorMsg('');
        return true;
      }
      if (hasUpper) {
        if (trimmedText.length >= 3) {
          const hasDash = trimmedText.charAt(1) === '-';
          const hasNumbers = [...trimmedText.substring(2)].reduce(
            (acc, c) => acc && !isNaN(Number(c)),
            true,
          );
          if (hasDash && hasNumbers && func(trimmedText)) {
            setInValid(false);
            setErrorMsg('');
            return true;
          }
        }
      }
    }
    setInValid(true);
    setErrorMsg('Invalid state name');
    return false;
  };

  return (
    <Box
      sx={{
        display: 'flex',
        justifyContent: 'center',
        textAlign: 'center',
      }}
    >
      <Stack spacing={1}>
        <FormControl variant="standard" error={isInValid}>
          <InputLabel>State</InputLabel>
          <Input value={val} onChange={(e) => setValue(e.target.value)} />
          {errorMsg ? (
            <FormHelperText id="component-error-text">
              {errorMsg}
            </FormHelperText>
          ) : (
            <div></div>
          )}
        </FormControl>
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
          </RadioGroup>
        </FormControl>
        <Stack direction="row" spacing={2} style={{ justifyContent: 'center' }}>
          <Button
            size="small"
            variant="contained"
            color="primary"
            startIcon={<Add />}
            onClick={(_) => {
              if (
                validateState(
                  val,
                  (t) => !props.states.find((s) => s.name === t),
                )
              ) {
                props.onSubmit({ name: val, type: stateType });
                resetValue();
                props.toggleSnack(`State '${val.trim()}' added`);
              }
            }}
          >
            Add
          </Button>
          <Button
            size="small"
            variant="contained"
            color="primary"
            startIcon={<Delete />}
            onClick={(_) => {
              if (
                validateState(
                  val,
                  (t) => !!props.states.find((s) => s.name === t),
                )
              ) {
                props.onDelete({ name: val, type: stateType });
                resetValue();
                props.toggleSnack(`State '${val.trim()}' removed`);
              }
            }}
          >
            Remove
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
};

export default AddRemoveStateForm;
