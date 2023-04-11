import { useState, forwardRef } from 'react';
import {
  Dialog,
  Stack,
  Divider,
  IconButton,
  DialogTitle,
  DialogContent,
  Snackbar,
} from '@mui/material';
import MuiAlert, { AlertProps } from '@mui/material/Alert';
import { styled } from '@mui/material/styles';
import AddRemoveForm from './AddRemoveForm';
import AddRemoveStateForm from './AddRemoveStateForm';
import { useAddRemoveRuleForm } from './AddRemoveRuleForm';
import { State, FSMRule } from '../../types/machine';
import { Close as CloseIcon } from '@mui/icons-material';

type SnackState = {
  msg: string | undefined;
  open: boolean;
};

type MachineEditorModalProps = {
  isOpen: boolean;
  onClose: () => void;
  addState: (state: State) => void;
  removeState: (state: State) => void;
  states: State[];
};

const BootstrapDialog = styled(Dialog)(({ theme }) => ({
  '& .MuiDialogContent-root': {
    padding: theme.spacing(2),
  },
  '& .MuiDialogActions-root': {
    padding: theme.spacing(1),
  },
}));

const Alert = forwardRef<HTMLDivElement, AlertProps>(function Alert(
  props,
  ref,
) {
  return <MuiAlert elevation={6} ref={ref} variant="filled" {...props} />;
});

const MachineEditorModal = (props: MachineEditorModalProps) => {
  const AddRemoveRuleForm = useAddRemoveRuleForm('dfa');
  const [snack, setSnack] = useState<SnackState>({
    msg: undefined,
    open: false,
  });
  const toggleSnackWithMsg = (text: string) =>
    setSnack({ msg: text, open: !snack.open });
  const toggleSnack = () => setSnack({ msg: undefined, open: !snack.open });

  return (
    <BootstrapDialog open={props.isOpen} onClose={props.onClose}>
      <DialogTitle sx={{ m: 0, p: 2 }}> Edit Machine </DialogTitle>
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
      <Divider />
      <Stack spacing={3} paddingBottom={5}>
        <DialogContent>
          <AddRemoveStateForm
            onDelete={props.removeState}
            onSubmit={props.addState}
            states={props.states}
            toggleSnack={toggleSnackWithMsg}
          />
        </DialogContent>
        <Divider />
        <DialogContent>
          <AddRemoveForm
            label="Alpha"
            onDelete={(_) => false}
            onSubmit={(_) => false}
            validate={(_) => true}
          />
        </DialogContent>
        <Divider />
        <DialogContent>
          <AddRemoveRuleForm
            addRule={() => true}
            removeRule={() => true}
            rules={[] as FSMRule[]}
            alphabet={[] as string[]}
          />
        </DialogContent>
      </Stack>
      {snack.open && (
        <Snackbar
          open={snack.open}
          autoHideDuration={4000}
          onClose={toggleSnack}
        >
          <Alert
            onClose={toggleSnack}
            severity="success"
            sx={{ width: '100%' }}
          >
            {snack.msg}
          </Alert>
        </Snackbar>
      )}
    </BootstrapDialog>
  );
};

export default MachineEditorModal;
