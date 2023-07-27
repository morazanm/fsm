import {
  Alert,
  Breadcrumbs,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
} from '@mui/material';
import { Typography } from '@mui/material';
import { MachineType } from '../types/machine';

type BasicDialogProps = {
  onClose: () => void;
  open: boolean;
  title: string;
  body: string | JSX.Element;
  bodyStyle?: object;
};

export const BasicDialog = (props: BasicDialogProps) => {
  return (
    <Dialog
      open={props.open}
      onClose={props.onClose}
      aria-labelledby="alert-dialog-title"
      aria-describedby="alert-dialog-description"
    >
      <DialogTitle id="alert-dialog-title">{props.title}</DialogTitle>
      <DialogContent>
        <DialogContentText
          id="alert-dialog-description"
          style={props.bodyStyle}
        >
          {props.body}
        </DialogContentText>
      </DialogContent>
      <DialogActions>
        <Button onClick={props.onClose}>Close</Button>
      </DialogActions>
    </Dialog>
  );
};

type IncomingMachineDialogProps = {
  open: boolean;
  incomingType: MachineType;
  reject: () => void;
  accept: () => void;
};

export const IncomingMachineDialog = (props: IncomingMachineDialogProps) => {
  return (
    <Dialog
      open={props.open}
      aria-labelledby="alert-dialog-title"
      aria-describedby="alert-dialog-description"
    >
      <DialogTitle id="alert-dialog-title">{`Incoming Prebuilt Machine: ${props.incomingType}`}</DialogTitle>
      <DialogContent>
        <DialogContentText id="alert-dialog-description">
          {`Received a request to import a prebuilt ${props.incomingType}. If you wish to import this machine press accept. If you do not wish to import this machine press reject.`}
        </DialogContentText>
        <br />
        <DialogContent id="alert-dialog-description">
          <Alert severity="warning">
            Pressing accept will remove any machine that is currently loaded in
            the GUI. You can save your current machine using the following
            steps:
            <Breadcrumbs
              sx={{
                display: 'flex',
                justifyContent: 'center',
                alignItems: 'center',
                paddingTop: '5px',
              }}
              aria-label="breadcrumb"
              separator="›››"
            >
              <Typography color="text.secondary">File</Typography>
              <Typography color="text.secondary">Save</Typography>
            </Breadcrumbs>
          </Alert>
          <br />
        </DialogContent>
      </DialogContent>
      <DialogActions>
        <Button onClick={props.reject}>Reject</Button>
        <Button onClick={props.accept}>Accept</Button>
      </DialogActions>
    </Dialog>
  );
};
