# React Components for Mainframe CI/CD Modernization Playbook

This directory contains React components that can be used to visualize various aspects of the Mainframe CI/CD Modernization process.

## Components

### AgentBasedModernizationFramework

A component that visualizes the agent-based approach to mainframe modernization, showing how different specialized agents work together in the modernization process.

#### Features

- Responsive SVG diagram that scales to container width
- Tailwind CSS styling for modern appearance
- Comprehensive visualization of agents, their connections, and workflow

#### Usage

```jsx
import React from 'react';
import AgentBasedModernizationFramework from './AgentBasedModernizationFramework';

function MyPage() {
  return (
    <div>
      <h1>Mainframe Modernization</h1>
      <AgentBasedModernizationFramework />
    </div>
  );
}
```

## Requirements

These components require:

- React 16.8+ (for hooks support)
- Tailwind CSS 2.0+

## Integration

To integrate these components into your application:

1. Ensure React and Tailwind CSS are set up in your project
2. Copy the component files to your project's component directory
3. Import and use the components as shown in the examples

You can also customize the components by modifying their source code to match your specific requirements and branding.

## Example

See `example-usage.jsx` for a complete example of how to use these components in a page. 