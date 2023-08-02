import React from 'react';
import whyDidYouRender from '@welldone-software/why-did-you-render';

if (process.env.NODE_ENV === 'development') {
	whyDidYouRender(React, {
		trackAllPureComponents: true,
		trackHooks: true,
		include: [/.*/],
		collapseGroups: true,
		diffNameColor: 'darkturquoise',
		titleColor: 'lavender',
	});
}
