import { useEffect, useState } from 'react';
import { LogLoc, log } from 'lib/logging';

const usePort = () => {
	const [port, setPort] = useState<chrome.runtime.Port>(
		chrome.runtime.connect({ name: 'ws' })
	);

	useEffect(() => {
		port.onDisconnect.addListener(() => {
			log(LogLoc.NEWTAB, 'Port disconnected, reconnecting...');
			setPort(chrome.runtime.connect({ name: 'ws' }));
		});
		return () => {
			port.disconnect();
		};
	}, [port]);

	return port;
};

export default usePort;
