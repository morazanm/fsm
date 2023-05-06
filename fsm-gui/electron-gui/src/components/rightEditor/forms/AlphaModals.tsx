import { useState } from 'react';
import {
  Dialog,
  IconButton,
  DialogTitle,
  DialogContent,
  FormControl,
  FormHelperText,
  Button,
  DialogActions,
  TextField,
  DialogContentText,
} from '@mui/material';
import { useTheme } from '@mui/material/styles';
import { Close as CloseIcon } from '@mui/icons-material';
import { styled } from '@mui/material/styles';
import { FSMAlpha, FSMStackAlpha } from '../../../types/machine';

const BootstrapDialog = styled(Dialog)(({ theme }) => ({
  '& .MuiDialogContent-root': {
    padding: theme.spacing(2),
  },
  '& .MuiDialogActions-root': {
    padding: theme.spacing(1),
  },
}));

// A list of numbers or lowercase symbols with a string representation of length one not including EMP.
const validateText = (value: string): boolean => {
  const trimmedValue = value.trim();
  return !!trimmedValue.match(/^[0-9a-z]+$/);
};

type AlphaModalProps = {
  alpha: FSMAlpha[];
  setAlpha: (alpha: FSMAlpha[]) => void;
  isOpen: boolean;
  toggle: () => void;
  toggleSnack: (text: string) => void;
};

export const AlphaModal = (props: AlphaModalProps) => {
  const theme = useTheme();
  const [input, setInput] = useState('');
  const [errorMsg, setErrorMsg] = useState(null);

  return (
    <BootstrapDialog open={props.isOpen} onClose={props.toggle} theme={theme}>
      <DialogTitle sx={{ m: 0, p: 2 }}>Edit Alphabet</DialogTitle>
      <IconButton
        aria-label="close"
        onClick={props.toggle}
        sx={{
          position: 'absolute',
          right: 8,
          top: 8,
          color: (theme) => theme.palette.grey[500],
        }}
      >
        <CloseIcon />
      </IconButton>
      <DialogContent>
        <DialogContentText>
          Please enter a letter(s) that you wish to add/remove from the
          alphabet. You can use spaces to add/remove more then one value at a
          time.
        </DialogContentText>
        <FormControl variant="standard" error={!!errorMsg}>
          <TextField
            autoFocus
            fullWidth
            margin="dense"
            id="name"
            label="Alphabet Letter(s)"
            type="text"
            variant="standard"
            error={!!errorMsg}
            onChange={(e) => setInput(e.target.value)}
          />
          {errorMsg ? (
            <FormHelperText id="component-error-text">
              {errorMsg}
            </FormHelperText>
          ) : (
            <div></div>
          )}
        </FormControl>
      </DialogContent>
      <DialogActions>
        <Button
          onClick={() => {
            const values = input.trim().split(' ');
            const invalid_alpha = values.filter(
              (v) => !props.alpha.includes(v),
            );
            if (invalid_alpha.length > 0) {
              setErrorMsg(`"${invalid_alpha}" are not in the alphabet`);
            } else {
              const new_alpha = props.alpha.filter((a) => !values.includes(a));
              props.setAlpha(new_alpha);
              props.toggleSnack(
                `${values} successfully removed from the alphabet`,
              );
              props.toggle();
            }
          }}
        >
          Remove
        </Button>
        <Button
          onClick={() => {
            const values = input.trim().split(' ');
            const alpha_to_add = values.filter((v) => !props.alpha.includes(v));
            const is_valid = alpha_to_add.reduce(
              (a, v) => a && validateText(v),
              true,
            );

            if (is_valid) {
              props.setAlpha([...props.alpha, ...alpha_to_add]);
              props.toggleSnack(
                `${alpha_to_add} successfully added to the alphabet`,
              );
              props.toggle();
            } else {
              setErrorMsg(
                'Invalid alphabet symbol. A valid alpha is a list of numbers or lowercase symbols with a string representation of length one not including EMP.',
              );
            }
          }}
        >
          Add
        </Button>
      </DialogActions>
    </BootstrapDialog>
  );
};

type GammaModalProps = {
  stackAlpha: FSMStackAlpha[];
  setStackAlpha: (stackAlpha: FSMStackAlpha[]) => void;
  isOpen: boolean;
  toggle: () => void;
  toggleSnack: (text: string) => void;
};

export const GammaModal = (props: GammaModalProps) => {
  const theme = useTheme();
  const [input, setInput] = useState('');
  const [errorMsg, setErrorMsg] = useState(null);

  return (
    <BootstrapDialog open={props.isOpen} onClose={props.toggle} theme={theme}>
      <DialogTitle sx={{ m: 0, p: 2 }}>Edit Stack Alphabet</DialogTitle>
      <IconButton
        aria-label="close"
        onClick={props.toggle}
        sx={{
          position: 'absolute',
          right: 8,
          top: 8,
          color: (theme) => theme.palette.grey[500],
        }}
      >
        <CloseIcon />
      </IconButton>
      <DialogContent>
        <DialogContentText>
          Please enter a letter(s) that you wish to add/remove from the stack
          alphabet. You can use spaces to add/remove more then one value at a
          time.
        </DialogContentText>
        <FormControl variant="standard" error={!!errorMsg}>
          <TextField
            autoFocus
            fullWidth
            margin="dense"
            id="name"
            label="Stack Alphabet Letter(s)"
            type="text"
            variant="standard"
            error={!!errorMsg}
            onChange={(e) => setInput(e.target.value)}
          />
          {errorMsg ? (
            <FormHelperText id="component-error-text">
              {errorMsg}
            </FormHelperText>
          ) : (
            <div></div>
          )}
        </FormControl>
      </DialogContent>
      <DialogActions>
        <Button
          onClick={() => {
            const values = input.trim().split(' ');
            const invalid_alpha = values.filter(
              (v) => !props.stackAlpha.includes(v),
            );
            if (invalid_alpha.length > 0) {
              setErrorMsg(`"${invalid_alpha}" are not in the stack alphabet`);
            } else {
              const new_alpha = props.stackAlpha.filter(
                (a) => !values.includes(a),
              );
              props.setStackAlpha(new_alpha);
              props.toggleSnack(
                `${values} successfully removed from the stack alphabet`,
              );
              props.toggle();
            }
          }}
        >
          Remove
        </Button>
        <Button
          onClick={() => {
            const values = input.trim().split(' ');
            const alpha_to_add = values.filter(
              (v) => !props.stackAlpha.includes(v),
            );
            const is_valid = alpha_to_add.reduce(
              (a, v) => a && validateText(v),
              true,
            );
            if (is_valid) {
              props.setStackAlpha([...props.stackAlpha, ...alpha_to_add]);
              props.toggleSnack(
                `${alpha_to_add} successfully added to the stack alphabet`,
              );
              props.toggle();
            } else {
              setErrorMsg(
                'Invalid stack alphabet symbol. A valid stack alpha is a list of numbers or lowercase symbols with a string representation of length one not including EMP.',
              );
            }
          }}
        >
          Add
        </Button>
      </DialogActions>
    </BootstrapDialog>
  );
};
