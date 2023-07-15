import { useState } from 'react';

type UseMasterWS = () => [boolean, (amMasterWS: boolean) => void];

const useMasterWS: UseMasterWS = () => {
	const [amMasterWS, setAmMasterWS] = useState(false);

	return [amMasterWS, setAmMasterWS];
};

export default useMasterWS;
