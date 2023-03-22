import React from 'react';
import ReactDOM from 'react-dom';
import Button from '@mui/material/Button';

const App = () => {
  return (
    <div>
      <p>Hello from React</p>
      <Button variant="contained">Click Me</Button>
    </div>
  );
};

ReactDOM.render(<App />, document.body);
