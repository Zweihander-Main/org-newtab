import classNames from 'classnames';
import * as styles from './style.module.css';

type ButtonProps = {
	children: React.ReactNode;
	style: 'primary' | 'reset';
	className?: string;
	type?: 'button' | 'submit' | 'reset';
};

const Button: React.FC<ButtonProps> = ({
	children,
	style,
	className,
	type = 'button',
}) => {
	return (
		<button
			className={classNames(className, styles.button, {
				[styles.primary]: style === 'primary',
				[styles.reset]: style === 'reset',
			})}
			type={type}
		>
			{children}
		</button>
	);
};

export default Button;
