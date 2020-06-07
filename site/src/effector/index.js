import { createEffect, createEvent, createStore } from 'effector';

import mapData from '../data/map.json';

const setRawData = createEvent();
export const rawData = createStore([])
  .on(setRawData, (state, data) => data);

const setRawGeo = createEvent();
export const rawGeo = createStore([])
  .on(setRawGeo, (state, data) => data);

const setHeaders = createEvent();
export const headers = createStore([])
  .on(setHeaders, (state, data) => data);

export const setRowSelected = createEvent();
export const rowSelected = createStore('')
  .on(setRowSelected, (state, data) => data);

const setGeoMap = createEvent();
export const geoMap = createStore({})
  .on(setGeoMap, (state, data) => data);

export const initFx = createEffect('init').use(() => {
  setGeoMap(
    mapData.layers.reduce((acc, { id, name }) => {
      acc[id] = name;
      return acc;
    }, {})
  );

  setHeaders([
    {
      name: 'Region',
      selector: 'region',
    },
    {
      name: 'Votes',
      selector: 'votes',
      sortable: true,
    },
  ]);

  setRawData([
    { region: 'ru-al', votes: 100 },
    { region: 'ua-43', votes: 4 },
    { region: 'ru-zab', votes: 15 },
    { region: 'ru-yev', votes: 60 },
    { region: 'ru-tom', votes: 33 },
  ]);

  setRawGeo([]);
});
