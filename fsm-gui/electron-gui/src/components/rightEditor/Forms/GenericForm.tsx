import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogTitle from '@mui/material/DialogTitle';

type GenericFormProps = {
  title: string;
  children: JSX.Element;
  isOpen: boolean;
  onSubmit: <T>(val: T) => void;
  onClose: () => void;
  submitText?: string;
  closeText?: string;
};

export default function GenericForm(props: GenericFormProps) {
  return (
    <div>
      <Dialog open={props.isOpen} onClose={props.onClose}>
        <DialogTitle>{props.title}</DialogTitle>
        {props.children}
        <DialogActions>
          <Button onClick={props.onClose}>{props.closeText || 'Cancel'}</Button>
          <Button onClick={props.onSubmit}>
            {props.submitText || 'Submit'}
          </Button>
        </DialogActions>
      </Dialog>
    </div>
  );
}
