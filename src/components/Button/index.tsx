import classNames from 'classnames';
import * as styles from './style.module.css';

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
	styleType: 'primary' | 'reset';
	small?: boolean;
}

const Button: React.FC<ButtonProps> = ({
	styleType,
	small,
	children,
	className,
	...props
}) => {
	return (
		<button
			{...props}
			className={classNames(className, styles.button, {
				[styles.primary]: styleType === 'primary',
				[styles.reset]: styleType === 'reset',
				[styles.small]: small,
			})}
		>
			{children}
		</button>
	);
};

export default Button;
