import { useState, forwardRef } from 'react';
import {
  Stack,
  Typography,
  Box,
  ButtonGroup,
  IconButton,
  Divider,
  Snackbar,
} from '@mui/material';

import MuiAlert, { AlertProps } from '@mui/material/Alert';
import { Delete as DeleteIcon, Add as AddIcon } from '@mui/icons-material';
import { FSMAlpha } from '../../types/machine';
import { AddAlphaModal, DeleteAlphaModal } from './AlphaModals';

type SnackState = {
  msg: string | undefined;
  open: boolean;
};

type LeftEditorProps = {
  alpha: FSMAlpha[];
  addAlpha: (alpha: FSMAlpha) => void;
  removeAlpha: (alphas: FSMAlpha[]) => void;
};

type OpenModal = 'add' | 'remove' | null;

const Alert = forwardRef<HTMLDivElement, AlertProps>(function Alert(
  props,
  ref,
) {
  return <MuiAlert elevation={6} ref={ref} variant="filled" {...props} />;
});

const LeftEditor = (props: LeftEditorProps) => {
  const [openModal, setOpenModal] = useState<OpenModal>(null);
  const [snack, setSnack] = useState<SnackState>({
    msg: undefined,
    open: false,
  });

  const toggleSnackWithMsg = (text: string) =>
    setSnack({ msg: text, open: !snack.open });
  const toggleSnack = () => setSnack({ msg: undefined, open: !snack.open });

  const closeModal = () => setOpenModal(null);
  return (
    <>
      <Stack direction="column" alignItems="center" overflow="auto">
        <Typography variant="h5">Σ</Typography>
        <ButtonGroup variant="outlined">
          <IconButton
            aria-label="Add Alphabet"
            color="primary"
            onClick={() => setOpenModal('add')}
          >
            <AddIcon />
          </IconButton>
          <IconButton
            aria-label="Remove Alphabet"
            color="primary"
            onClick={() => setOpenModal('remove')}
          >
            <DeleteIcon />
          </IconButton>
        </ButtonGroup>
        <Divider orientation="horizontal" flexItem />
        <Stack direction="column" overflow="auto" style={{ maxHeight: '60vh' }}>
          {props.alpha.map((a) => (
            <Box sx={{ paddingRight: '10px', paddingLeft: '10px' }}>
              <Typography variant="h6" display="flex" alignItems="center">
                {a}
              </Typography>
            </Box>
          ))}
        </Stack>
      </Stack>
      {openModal === 'add' && (
        <AddAlphaModal
          alpha={props.alpha}
          addAlpha={props.addAlpha}
          isOpen={openModal === 'add'}
          onClose={closeModal}
          toggleSnack={toggleSnackWithMsg}
        />
      )}

      {openModal === 'remove' && (
        <DeleteAlphaModal
          alpha={props.alpha}
          deleteAlpha={props.removeAlpha}
          isOpen={openModal === 'remove'}
          onClose={closeModal}
          toggleSnack={toggleSnackWithMsg}
        />
      )}
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
    </>
  );
};

export default LeftEditor;
