import {
  Dialog,
  DialogContent,
  DialogContentText,
  DialogTitle,
  IconButton,
  useTheme,
} from '@mui/material';
import { Close as CloseIcon } from '@mui/icons-material';
import { State } from '../../types/machine';

type StateModalProps = {
  open: boolean;
  onClose: () => void;
  state: State;
};

export const StateModal = (props: StateModalProps) => {
  const theme = useTheme();
  return (
    <Dialog
      open={props.open}
      keepMounted
      maxWidth="sm"
      onClose={props.onClose}
      aria-describedby="alert-dialog-slide-description"
    >
      <DialogTitle>
        {`State: ${props.state.name}`}
        <IconButton
          aria-label="close"
          onClick={props.onClose}
          sx={{
            position: 'absolute',
            right: 8,
            top: 8,
            color: theme.palette.grey[500],
          }}
        >
          <CloseIcon />
        </IconButton>
      </DialogTitle>
      <DialogContent>TODO</DialogContent>
    </Dialog>
  );
};
