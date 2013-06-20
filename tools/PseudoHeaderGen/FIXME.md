The output isn't 100% perfect yet, some special cases are still not handled by PHG's improved DeclPrinter and TypePrinter:

	// Base STL allocator class.
	template<typename T>
	struct STLAllocatorBase
	{	// base class for generic allocators
		typedef T value_type;
	};

	// Base STL allocator class. (const T version).
	template<typename T>
	struct STLAllocatorBase<const T>
	{	// base class for generic allocators for const T
		typedef T value_type;
	};

(OgreMemorySTLAllocator.h)

I have no idea what template\<typename T\> struct STLAllocatorBase\<const T\> exactly is
in C++ terms (is it a template specialization?). I should ask Ogre devs.
Vanilla DeclPrinter only prints:

	struct STLAllocatorBase { ... };

which is obviously incorrect.
