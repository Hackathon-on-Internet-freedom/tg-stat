import React from 'react';
import DataTable from 'react-data-table-component';

import style from './style.module.scss';

import { geoMap, headers, rawData, rowSelected, setRowSelected } from '../../effector';
import { useStore } from 'effector-react';

const Table = () => {
  const colHeaders = useStore(headers);
  const data = useStore(rawData);
  const selected = useStore(rowSelected);
  const geoNames = useStore(geoMap);

  function onClick(row) {
    const newSelected = row.region;
    setRowSelected(newSelected === selected ? '' : newSelected);
  }

  const conditionalRowStyles = [
    {
      when: row => row.region === selected,
      style: {
        backgroundColor: '#eaa',
      },
    },
  ];

  return (
    <div className={style.root}>
      <DataTable
        title={'Votes' + (selected ? ' of ' + geoNames[selected] : '')}
        columns={colHeaders}
        data={data}
        theme="dark"
        highlightOnHover
        pointerOnHover
        conditionalRowStyles={conditionalRowStyles}
        onRowClicked={onClick}
      />
    </div>
  );
};

export default Table;
