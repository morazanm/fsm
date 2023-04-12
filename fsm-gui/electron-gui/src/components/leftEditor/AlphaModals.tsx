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
  InputLabel,
  DialogContentText,
  Select,
  OutlinedInput,
  SelectChangeEvent,
  Box,
  Chip,
  MenuItem,
} from '@mui/material';
import { Theme, useTheme } from '@mui/material/styles';
import { Close as CloseIcon, Cancel as CancelIcon } from '@mui/icons-material';
import { styled } from '@mui/material/styles';
import { FSMAlpha } from '../../types/machine';

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

type AddAlphaProps = {
  alpha: FSMAlpha[];
  addAlpha: (alpha: FSMAlpha) => void;
  isOpen: boolean;
  onClose: () => void;
  toggleSnack: (text: string) => void;
};

export const AddAlphaModal = (props: AddAlphaProps) => {
  const [input, setInput] = useState('');
  const [errorMsg, setErrorMsg] = useState(null);

  return (
    <BootstrapDialog open={props.isOpen} onClose={props.onClose}>
      <DialogTitle sx={{ m: 0, p: 2 }}>Add Alphabet</DialogTitle>
      <IconButton
        aria-label="close"
        onClick={props.onClose}
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
          Please enter a letter that you wish to add the the alphabet
        </DialogContentText>
        <FormControl variant="standard" error={!!errorMsg}>
          <TextField
            autoFocus
            margin="dense"
            id="name"
            label="Alphabet Letter"
            type="email"
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
        <Button onClick={props.onClose}>Cancel</Button>
        <Button
          onClick={() => {
            const val = input.trim();
            if (props.alpha.includes(val)) {
              setErrorMsg(`${val} is already in the alphebet`);
            } else if (validateText(val)) {
              props.addAlpha(val);
              props.toggleSnack(
                `${val} was successfully added the the alphabet`,
              );
              props.onClose();
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

const ITEM_HEIGHT = 48;
const ITEM_PADDING_TOP = 8;
const MenuProps = {
  PaperProps: {
    style: {
      maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
      width: 250,
    },
  },
};

type DeleteAlphaProps = {
  alpha: FSMAlpha[];
  deleteAlpha: (alphas: FSMAlpha[]) => void;
  isOpen: boolean;
  onClose: () => void;
  toggleSnack: (text: string) => void;
};

function getStyles(name: string, personName: readonly string[], theme: Theme) {
  return {
    fontWeight:
      personName.indexOf(name) === -1
        ? theme.typography.fontWeightRegular
        : theme.typography.fontWeightMedium,
  };
}

export const DeleteAlphaModal = (props: DeleteAlphaProps) => {
  const theme = useTheme();
  const [selectedAlphas, setSelectedAlphas] = useState<FSMAlpha[]>([]);
  const [errorMsg, setErrorMsg] = useState(null);

  const handleChange = (event: SelectChangeEvent<typeof selectedAlphas>) => {
    const {
      target: { value },
    } = event;
    setSelectedAlphas(
      // On autofill we get a stringified value.
      typeof value === 'string' ? value.split(',') : value,
    );
  };

  const removeChip = (val: string) => {
    setSelectedAlphas(selectedAlphas.filter((v) => v !== val));
  };
  return (
    <BootstrapDialog open={props.isOpen} onClose={props.onClose}>
      <DialogTitle sx={{ m: 0, p: 2 }}>Delete Alphabet</DialogTitle>
      <IconButton
        aria-label="close"
        onClick={props.onClose}
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
          Please seletc the values you wish to delete
        </DialogContentText>

        <FormControl sx={{ m: 1, width: 300 }} error={!!errorMsg}>
          <InputLabel id="demo-multiple-chip-label">Chip</InputLabel>
          <Select
            labelId="demo-multiple-chip-label"
            id="demo-multiple-chip"
            multiple
            value={selectedAlphas}
            onChange={handleChange}
            input={<OutlinedInput id="select-multiple-chip" label="Chip" />}
            renderValue={(selected) => (
              <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                {selected.map((value) => (
                  <Chip
                    clickable
                    key={value}
                    value={value}
                    label={value}
                    variant="outlined"
                    color="primary"
                    deleteIcon={
                      <CancelIcon
                        onMouseDown={(e: any) => e.stopPropagation()}
                      />
                    }
                    onDelete={(_) => removeChip(value)}
                  />
                ))}
              </Box>
            )}
            MenuProps={MenuProps}
          >
            {props.alpha.map((a) => (
              <MenuItem
                key={a}
                value={a}
                style={getStyles(a, selectedAlphas, theme)}
              >
                {a}
              </MenuItem>
            ))}
          </Select>
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
        <Button onClick={props.onClose}>Cancel</Button>
        <Button
          onClick={() => {
            if (selectedAlphas.length === 0) {
              setErrorMsg(`Please select alphebet values to delete`);
            } else {
              props.deleteAlpha(selectedAlphas);
              props.toggleSnack(`${selectedAlphas} were successfully deleted`);
              props.onClose();
            }
          }}
        >
          Delete
        </Button>
      </DialogActions>
    </BootstrapDialog>
  );
};
