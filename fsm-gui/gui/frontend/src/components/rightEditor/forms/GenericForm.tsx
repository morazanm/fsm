import { useTheme } from '@mui/material';
import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogTitle from '@mui/material/DialogTitle';
import IconButton from '@mui/material/IconButton';
import CloseIcon from '@mui/icons-material/Close';

type GenericFormProps = {
  title: string;
  children: JSX.Element;
  isOpen: boolean;
  onSubmit: <T>(val: T) => void;
  onClose: () => void;
  // below are optional customizations
  submitText?: string;
  closeText?: string;
  onCancel?: () => void;
};

export default function GenericForm(props: GenericFormProps) {
  const theme = useTheme();
  return (
    <div>
      <Dialog open={props.isOpen} onClose={props.onClose}>
        <DialogTitle>
          {props.title}
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
        {props.children}
        <DialogActions>
          <Button
            onClick={() =>
              props.onCancel ? props.onCancel() : props.onClose()
            }
          >
            {props.closeText || 'Cancel'}
          </Button>
          <Button onClick={props.onSubmit}>
            {props.submitText || 'Submit'}
          </Button>
        </DialogActions>
      </Dialog>
    </div>
  );
}
