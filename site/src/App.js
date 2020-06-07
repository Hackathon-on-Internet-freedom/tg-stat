import React, { useEffect } from 'react';

import style from './App.module.scss';

import { initFx } from './effector';

import Map from './components/Map';
import Table from './components/Table';

const App = () => {
  useEffect(() => {
    initFx();
  }, []);

  return (
    <div className={style.root}>
      <Map />

      <Table />
    </div>
  );
}

export default App;
