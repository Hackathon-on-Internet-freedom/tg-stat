import React from 'react';
import { VectorMap } from '@south-paw/react-vector-maps';
import { useStore } from 'effector-react';

import mapData from '../../data/map.json';
import { geoMap, rowSelected, setRowSelected } from '../../effector';

import style from './style.module.scss';

const Map = () => {
  const geoNames = useStore(geoMap);

  const selected = useStore(rowSelected);

  function onClick({ target }) {
    const newSelected = target.attributes.id.value;
    setRowSelected(newSelected === selected ? '' : newSelected);
  }

  return (
    <div className={style.root}>
      <VectorMap
        {...mapData}
        currentLayers={selected ? [selected] : []}
        layerProps={{ onClick }}
      />
    </div>
  );
};

export default Map;
