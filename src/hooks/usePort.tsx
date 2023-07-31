import { useEffect, useState } from 'react';
import { LogLoc, log } from 'lib/logging';

const usePort = () => {
	const [port, setPort] = useState<chrome.runtime.Port>(
		chrome.runtime.connect({ name: 'ws' })
	);

	useEffect(() => {
		const handlePortDisconnect = () => {
			log(LogLoc.NEWTAB, 'Port disconnected, reconnecting...');
			setPort(chrome.runtime.connect({ name: 'ws' }));
		};
		if (!port.onDisconnect.hasListener(handlePortDisconnect)) {
			port.onDisconnect.addListener(handlePortDisconnect);
		}
		return () => {
			port.onDisconnect.removeListener(handlePortDisconnect);
			port.disconnect();
		};
	}, [port]);

	return port;
};

export default usePort;
