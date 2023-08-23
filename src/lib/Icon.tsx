import { memo } from 'react';

type IconProps = {
	icon: React.ReactNode;
};

const Icon: React.FC<IconProps> = ({ icon }) => {
	return <>{icon}</>;
};

const MemoizedIcon = memo(Icon, () => true);

export default MemoizedIcon;
