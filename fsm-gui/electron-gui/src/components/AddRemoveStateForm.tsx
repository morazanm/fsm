import React, { useState } from 'react';
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

type StateType = 'start' | 'final' | 'normal';

type AddRemoveProps = {
  onDelete: (name: string, kind: StateType) => boolean;
  onSubmit: (name: string, kind: StateType) => boolean;
  // The returned string is the text that is used in the error msg display
  validate: (name: string, kind: StateType) => true | string;
};

// form for adding and removing adding and removing states from the GUI
const AddRemoveStateForm = (props: AddRemoveProps) => {
  const [val, setValue] = useState('');
  const [errorMsg, setErrorMsg] = useState('');
  const [isInValid, setInValid] = useState(false);
  const [stateType, setType] = useState<StateType>('start');
  const resetValue = () => setValue('');

  const validateText = (text: string) => {
    const res = props.validate(text, stateType);
    if (typeof res === 'string') {
      setInValid(true);
      setErrorMsg(res);
      return false;
    }
    setInValid(false);
    setErrorMsg('');
    return true;
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
          <Input onChange={(e) => setValue(e.target.value)} />
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
              if (validateText(val)) {
                props.onSubmit(val, stateType);
                resetValue();
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
              if (validateText(val)) {
                props.onDelete(val, stateType);
                resetValue();
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
