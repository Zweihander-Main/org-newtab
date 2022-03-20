import { useEffect, useState } from 'react';
import './App.css';

const OrgClock: React.FC = () => {
	const [clockedData, setClockedData] = useState(
		'No clocked data received from Emacs.'
	);

	useEffect(() => {
		// call websocket
	}, [setClockedData]);

	return <p>{clockedData}</p>;
};

function App() {
	return (
		<div className="app">
			<header className="app-header">
				<OrgClock />
			</header>
		</div>
	);
}

export default App;
