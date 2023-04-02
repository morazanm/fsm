import React, { useState } from 'react';
import { Delete as DeleteIcon, Add as AddIcon } from '@mui/icons-material';
import {
  Button,
  Input,
  FormControl,
  FormHelperText,
  InputLabel,
  Box,
  Stack,
} from '@mui/material';

type AddRemoveProps = {
  label: string;
  onDelete: (val: string) => boolean;
  onSubmit: (val: string) => boolean;
  // The returned string is the text that is used in the error msg display
  validate: (val: string) => true | string;
};

// Generaic Add/Remove form
const AddRemoveForm = (props: AddRemoveProps) => {
  const [val, setValue] = useState('');
  const [errorMsg, setErrorMsg] = useState('');
  const [isInValid, setInValid] = useState(false);
  const resetValue = () => setValue('');

  const validateText = (text: string) => {
    const res = props.validate(text);
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
          <InputLabel>{props.label}</InputLabel>
          <Input onChange={(e) => setValue(e.target.value)} />
          {errorMsg ? (
            <FormHelperText id="component-error-text">
              {errorMsg}
            </FormHelperText>
          ) : (
            <div></div>
          )}
        </FormControl>
        <Stack direction="row" spacing={2} style={{ justifyContent: 'center' }}>
          <Button
            size="small"
            variant="contained"
            color="primary"
            startIcon={<AddIcon />}
            onClick={(_) => {
              if (validateText(val)) {
                props.onSubmit(val);
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
            startIcon={<DeleteIcon />}
            onClick={(_) => {
              if (validateText(val)) {
                props.onDelete(val);
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

export default AddRemoveForm;
