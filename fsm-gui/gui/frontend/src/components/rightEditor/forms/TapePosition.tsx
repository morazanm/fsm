import { useState } from 'react';
import TextField from '@mui/material/TextField';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import GenericForm from './GenericForm';
import { FSMAlpha } from '../../../types/machine';
import { useTheme } from '@mui/material';

type TapePositionModalProps = {
  isOpen: boolean;
  input: FSMAlpha[];
  toggle: () => void;
  tapePosition: number;
  setTapePosition: (position: number) => void;
  toggleSnack: (text: string) => void;
};

const TapePositionModal = (props: TapePositionModalProps) => {
  const theme = useTheme();
  const [data, setData] = useState('');
  const [error, setError] = useState('');

  const onSubmit = (incoming: string) => {
    const parsedNumber = Number(incoming);
    if (!isNaN(parsedNumber)) {
      if (parsedNumber < 0) {
        setError('Number must be >= 0');
      } else if (parsedNumber >= props.input.length) {
        setError('Number is outside the range of the current input');
      } else {
        props.setTapePosition(parsedNumber);
        props.toggle();
        props.toggleSnack('Tape position was successfully set');
      }
    } else {
      setError('Invalid number supplied');
    }
  };
  return (
    <GenericForm
      title="Tape Position"
      isOpen={props.isOpen}
      onClose={() => {
        setError('');
        props.toggle();
      }}
      onSubmit={() => onSubmit(data)}
    >
      <DialogContent>
        <DialogContentText>
          Please enter a number for where you would like the machine to start
          from.
          <br />
          The current index is:{' '}
          <span style={{ color: theme.palette.primary.main }}>
            {props.tapePosition}
          </span>
        </DialogContentText>
        <TextField
          autoFocus
          error={error !== ''}
          margin="dense"
          id="name"
          label="Position"
          type="number"
          fullWidth
          variant="standard"
          helperText={error}
          onChange={(e) => setData(e.target.value)}
        />
      </DialogContent>
    </GenericForm>
  );
};

export default TapePositionModal;
